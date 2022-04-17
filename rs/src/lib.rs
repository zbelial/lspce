#![allow(unused)]

use emacs::{defun, Env, IntoLisp, Result, Value};

use std::{
    collections::HashMap,
    fmt::{format, Debug},
    mem::MaybeUninit,
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
    slice::SliceIndex,
    sync::{mpsc, Arc, Mutex, Once},
    thread::{self, JoinHandle, Thread},
};

pub trait Transport {
    fn read(&mut self); // 读一个完整的响应/通知
    fn write(&mut self, buf: &str);
}

#[derive(Debug)]
struct SocketTransport {}

#[derive(Debug)]
struct IoTransport {
    pub stdin: ChildStdin,
    pub stdout: ChildStdout,
}

impl IoTransport {
    pub fn new(stdin: ChildStdin, stdout: ChildStdout) -> Self {
        IoTransport { stdin, stdout }
    }
}

impl Transport for IoTransport {
    fn read(&mut self) {
        todo!()
    }

    fn write(&mut self, buf: &str) {
        todo!()
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

fn initialize() {}

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
    if (has_server(env, root_uri.to_string(), file_type.to_string()).is_ok()) {
        return Ok("".to_string());
    }

    let mut server = LspServer::new(
        root_uri.clone(),
        cmd.clone(),
        cmd_args.clone(),
        lsp_args.clone(),
    );

    env.message(&format!("initialized."));

    Ok("".to_string())
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
