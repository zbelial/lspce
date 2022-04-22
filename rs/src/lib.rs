#![allow(unused)]

mod connection;
mod error;
mod msg;
mod socket;
mod stdio;

use bytes::Buf;
use bytes::BytesMut;
use connection::Connection;
use emacs::{defun, Env, IntoLisp, Result, Value};
use error::ParseError;
use lsp_types::lsp_request;
use lsp_types::request::Initialize;
use lsp_types::request::Request;
use lsp_types::ClientCapabilities;
use lsp_types::InitializeParams;
use lsp_types::InitializeResult;
use lsp_types::Url;
use memchr::memmem;
use msg::Message;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::result::Result as RustResult;
use std::str::Utf8Error;
use stdio::IoThreads;

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

#[derive(Debug)]
struct FileInfo {
    pub uri: String, // 文件名
}

struct LspServer {
    pub child: Option<Child>,
    pub nick_name: String,
    pub initialized: bool,               // 是否已经启动完
    pub capabilities: String,            // TODO 类型
    pub usable_capabilites: String,      // TODO
    pub uris: HashMap<String, FileInfo>, // key: uri
    transport: Option<Connection>,       // TODO StdioConnection替换为模板参数
    threads: Option<IoThreads>,
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
                threads: None,
            };

            let mut stdin = c.stdin.take().unwrap();
            let mut stdout = c.stdout.take().unwrap();

            let (mut transport, mut threads) = Connection::stdio(stdin, stdout);

            server.transport = Some(transport);
            server.threads = Some(threads);

            Some(server)
        } else {
            None
        }
    }

    pub fn request(&mut self, request: String) -> Option<String> {
        if self.transport.is_some() {
            let msg: Message = serde_json::from_str(&request).unwrap();

            let result = self.transport.as_ref().unwrap().write(msg);
        }

        None
    }

    pub fn notify(&mut self, notification: String) {}
}

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
    if (server_running(env, root_uri.clone(), file_type.clone()).unwrap()) {
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

fn initialize(env: &Env, root_uri: String, file_type: String, lsp_args: String) -> Result<String> {
    env.message(&format!("initialize"));

    // let uri = Url::parse(&root_uri)?;
    // let req_params = InitializeParams {
    //     process_id: None,
    //     root_uri: Some(uri),
    //     root_path: None,
    //     capabilities: ClientCapabilities {
    //         workspace: None,
    //         text_document: None,
    //         window: None,
    //         general: None,
    //         experimental: None,
    //     },
    //     workspace_folders: None,
    //     client_info: None,
    //     initialization_options: None,
    //     trace: None,
    //     locale: None,
    // };

    Ok("initialized".to_string())
}

#[defun]
fn server_running(env: &Env, root_uri: String, file_type: String) -> Result<bool> {
    let projects = projects().lock().unwrap();

    if let Some(p) = projects.get(&root_uri) {
        if let Some(s) = p.servers.get(&file_type) {
            return Ok(true);
        }
    }

    Ok(false)
}
