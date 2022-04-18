#![allow(unused)]

use bytes::Buf;
use bytes::BytesMut;
use emacs::{defun, Env, IntoLisp, Result, Value};
use lsp_types::lsp_request;
use lsp_types::request::Initialize;
use lsp_types::request::Request;
use lsp_types::ClientCapabilities;
use lsp_types::InitializeParams;
use lsp_types::InitializeResult;
use lsp_types::Url;
use memchr::memmem;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::io::Error as IoError;
use std::num::ParseIntError;
use std::result::Result as RustResult;
use std::str::Utf8Error;

use std::{
    collections::{HashMap, VecDeque},
    fmt::{format, Debug},
    io::{Read, Write},
    mem::MaybeUninit,
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
    slice::SliceIndex,
    sync::{
        mpsc::{self, Receiver, Sender},
        Arc, Mutex, Once,
    },
    thread::{self, JoinHandle, Thread},
};

pub trait Transport {
    fn read(&self) -> Option<String>; // 读一个完整的响应/通知
    fn write(&self, buf: &str);
}

#[derive(Debug)]
struct SocketTransport {}

#[derive(Debug)]
struct IoTransport {
    sender: Sender<String>,
    receiver: Receiver<String>,
    msgs: Arc<Mutex<VecDeque<String>>>,
}

/// Errors that can occur when processing an LSP message.
#[derive(Debug)]
pub enum ParseError {
    /// Failed to parse the JSON body.
    Body(serde_json::Error),
    /// Failed to encode the response.
    Encode(IoError),
    /// Failed to parse headers.
    Headers(httparse::Error),
    /// The media type in the `Content-Type` header is invalid.
    InvalidContentType,
    /// The length value in the `Content-Length` header is invalid.
    InvalidContentLength(ParseIntError),
    /// Request lacks the required `Content-Length` header.
    MissingContentLength,
    /// Request contains invalid UTF8.
    Utf8(Utf8Error),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ParseError::Body(ref e) => write!(f, "unable to parse JSON body: {}", e),
            ParseError::Encode(ref e) => write!(f, "failed to encode response: {}", e),
            ParseError::Headers(ref e) => write!(f, "failed to parse headers: {}", e),
            ParseError::InvalidContentType => write!(f, "unable to parse content type"),
            ParseError::InvalidContentLength(ref e) => {
                write!(f, "unable to parse content length: {}", e)
            }
            ParseError::MissingContentLength => {
                write!(f, "missing required `Content-Length` header")
            }
            ParseError::Utf8(ref e) => write!(f, "request contains invalid UTF8: {}", e),
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match *self {
            ParseError::Body(ref e) => Some(e),
            ParseError::Encode(ref e) => Some(e),
            ParseError::InvalidContentLength(ref e) => Some(e),
            ParseError::Utf8(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<serde_json::Error> for ParseError {
    fn from(error: serde_json::Error) -> Self {
        ParseError::Body(error)
    }
}

impl From<IoError> for ParseError {
    fn from(error: IoError) -> Self {
        ParseError::Encode(error)
    }
}

impl From<httparse::Error> for ParseError {
    fn from(error: httparse::Error) -> Self {
        ParseError::Headers(error)
    }
}

impl From<ParseIntError> for ParseError {
    fn from(error: ParseIntError) -> Self {
        ParseError::InvalidContentLength(error)
    }
}

impl From<Utf8Error> for ParseError {
    fn from(error: Utf8Error) -> Self {
        ParseError::Utf8(error)
    }
}

fn decode_headers(headers: &[httparse::Header<'_>]) -> RustResult<usize, ParseError> {
    let mut content_len = None;

    for header in headers {
        match header.name {
            "Content-Length" => {
                let string = std::str::from_utf8(header.value)?;
                let parsed_len = string.parse()?;
                content_len = Some(parsed_len);
            }
            "Content-Type" => {
                let string = std::str::from_utf8(header.value)?;
                let charset = string
                    .split(';')
                    .skip(1)
                    .map(|param| param.trim())
                    .find_map(|param| param.strip_prefix("charset="));

                match charset {
                    Some("utf-8") | Some("utf8") => {}
                    _ => return Err(ParseError::InvalidContentType),
                }
            }
            other => {}
        }
    }

    if let Some(content_len) = content_len {
        Ok(content_len)
    } else {
        Err(ParseError::MissingContentLength)
    }
}

impl IoTransport {
    pub fn new(mut stdin: ChildStdin, mut stdout: ChildStdout) -> Self {
        let (mut tx1, rx1) = mpsc::channel::<String>();
        let writer = thread::spawn(move || {
            for str in rx1 {
                stdin.write_all(str.as_bytes());
            }
        });

        let mut msgs: Arc<Mutex<VecDeque<String>>> = Arc::new(Mutex::new(VecDeque::new()));
        let msgs2 = msgs.clone();

        let (mut tx2, rx2) = mpsc::channel::<String>();
        let reader = thread::spawn(move || {
            let mut content_len: Option<usize> = None;
            let mut bytes_mut: BytesMut = BytesMut::with_capacity(65536);
            loop {
                if let Some(len) = content_len {
                    if (bytes_mut.len() < len) {
                        stdout.read(&mut bytes_mut);
                        continue;
                    }

                    let buf = &bytes_mut[..len];
                    let message = std::str::from_utf8(buf).unwrap();
                    if !message.is_empty() {
                        msgs2.lock().unwrap().push_back(message.to_string());
                    }

                    bytes_mut.advance(len);
                    content_len = None;
                } else {
                    stdout.read(&mut bytes_mut);

                    let mut dst = [httparse::EMPTY_HEADER; 2];
                    let (headers_len, headers) = match httparse::parse_headers(&bytes_mut, &mut dst)
                    {
                        Ok(p) => match p {
                            httparse::Status::Complete(output) => output,
                            httparse::Status::Partial => continue,
                        },
                        _ => {
                            continue;
                        }
                    };

                    match decode_headers(headers) {
                        Ok(len) => {
                            bytes_mut.advance(headers_len);
                            content_len = Some(len);
                            continue;
                        }
                        Err(err) => {
                            match err {
                                ParseError::MissingContentLength => {}
                                _ => bytes_mut.advance(headers_len),
                            }

                            // Skip any garbage bytes by scanning ahead for another potential message.
                            bytes_mut.advance(
                                memmem::find(&bytes_mut, b"Content-Length").unwrap_or_default(),
                            );
                        }
                    }
                }
            }
        });

        IoTransport {
            sender: tx1,
            receiver: rx2,
            msgs: Arc::new(Mutex::new(VecDeque::new())),
        }
    }
}

impl Transport for IoTransport {
    fn read(&self) -> Option<String> {
        self.msgs.lock().unwrap().pop_front()
    }

    fn write(&self, buf: &str) {
        self.sender.send(String::from(buf));
    }
}

#[derive(Debug)]
struct FileInfo {
    pub uri: String, // 文件名
}

#[derive(Debug)]
struct LspServer {
    pub child: Option<Child>,
    pub nick_name: String,
    pub initialized: bool,               // 是否已经启动完
    pub capabilities: String,            // TODO 类型
    pub usable_capabilites: String,      // TODO
    pub uris: HashMap<String, FileInfo>, // key: uri
    transport: Option<IoTransport>,      // TODO StdioConnection替换为模板参数
}

impl LspServer {
    pub fn new(
        root_uri: String,
        cmd: String,
        cmd_args: String,
        lsp_args: String,
    ) -> Option<LspServer> {
        let mut child = Command::new(cmd)
            .arg(cmd_args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn();

        if let Ok(mut c) = child {
            let mut server = LspServer {
                child: None,
                nick_name: "".to_string(),
                initialized: false,
                capabilities: "".to_string(),
                usable_capabilites: "".to_string(),
                uris: HashMap::new(),
                transport: None,
            };

            let mut stdin = c.stdin.take().unwrap();
            let mut stdout = c.stdout.take().unwrap();

            let mut transport = IoTransport::new(stdin, stdout);

            server.transport = Some(transport);

            Some(server)
        } else {
            None
        }
    }

    pub fn request(&mut self, request: String) -> Option<String> {
        if self.transport.is_some() {
            self.transport.as_ref().unwrap().write(&request);
        }

        None
    }

    pub fn notify(&mut self, notification: String) {}
}

#[derive(Debug)]
struct Project {
    pub root_uri: String,                    // 项目根目录
    pub servers: HashMap<String, LspServer>, // 每个LspServer处理一种类型的文件
    pub client_capabilities: String,         // TODO 类型
}

impl Project {
    pub fn new(root_uri: String, client_capabilities: String) -> Project {
        Project {
            root_uri,
            servers: HashMap::new(),
            client_capabilities,
        }
    }
}

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// let mut projects: Arc<Mutex<HashMap<String, Project>>> = Arc::new(Mutex::new(HashMap::new()));

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done loading!")
}

fn projects() -> &'static Arc<Mutex<HashMap<String, Project>>> {
    static mut PROJECTS: MaybeUninit<Arc<Mutex<HashMap<String, Project>>>> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();

    ONCE.call_once(|| unsafe {
        PROJECTS
            .as_mut_ptr()
            .write(Arc::new(Mutex::new(HashMap::new())))
    });

    unsafe { &*PROJECTS.as_mut_ptr() }
}

// Define a function callable by Lisp code.
#[defun]
fn say_hello(env: &Env, name: String) -> Result<Value<'_>> {
    env.message(&format!("Hello, {}!", name))
}

#[defun]
fn echo(env: &Env, content: String) -> Result<Value<'_>> {
    use std::process::Command;

    let output = Command::new("echo")
        .arg(content)
        .output()
        .expect("Failed to execute echo");

    env.message(&format!("Echo {:?}", String::from_utf8(output.stdout)))
}

#[defun]
fn connect(
    env: &Env,
    root_uri: String,
    file_type: String,
    cmd: String,
    cmd_args: String,
    lsp_args: String,
) -> Result<String> {
    env.message(&format!(
        "start initializing server for file_type {} in project {}",
        file_type, root_uri
    ));
    if (has_server(env, root_uri.clone(), file_type.clone()).unwrap()) {
        env.message(&format!(
            "server created for file_type {} in project {}",
            file_type, root_uri
        ));
        return Ok("server created.".to_string());
    }

    let mut server = LspServer::new(
        root_uri.clone(),
        cmd.clone(),
        cmd_args.clone(),
        lsp_args.clone(),
    );
    if let Some(s) = server {
        let mut projects = projects().lock().unwrap();
        let mut project = projects.get_mut(&root_uri);
        if let Some(p) = project.as_mut() {
            p.servers.insert(file_type, s);
        } else {
            let mut proj = Project::new(root_uri.clone(), "".to_string());
            proj.servers.insert(file_type, s);

            projects.insert(root_uri.clone(), proj);
        }

        env.message(&format!("initialized."));
    } else {
        env.message(&format!("connect failed"));
    }

    Ok("server created".to_string())
}

#[defun]
fn initialize(env: &Env, root_uri: String, file_type: String, lsp_args: String) -> Result<String> {
    env.message(&format!("initialize"));

    let uri = Url::parse(&root_uri)?;
    let reqParams = InitializeParams {
        process_id: None,
        root_uri: Some(uri),
        root_path: None,
        capabilities: ClientCapabilities {
            workspace: None,
            text_document: None,
            window: None,
            general: None,
            experimental: None,
        },
        workspace_folders: None,
        client_info: None,
        initialization_options: None,
        trace: None,
        locale: None,
    };

    Ok("initialized".to_string())
}

#[defun]
fn has_server(env: &Env, root_uri: String, file_type: String) -> Result<bool> {
    let projects = projects().lock().unwrap();

    if let Some(p) = projects.get(&root_uri) {
        if let Some(s) = p.servers.get(&file_type) {
            return Ok(true);
        }
    }

    Ok(false)
}
