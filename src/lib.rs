#![allow(unused)]

mod connection;
mod error;
mod logger;
mod msg;
mod socket;
mod stdio;

use bytes::Buf;
use bytes::BytesMut;
use chrono::Local;
use connection::Connection;
use crossbeam_channel::SendError;
use emacs::{defun, Env, IntoLisp, Result, Value};
use error::LspceError;
use logger::Logger;
use logger::LOG_ENABLE;
use logger::LOG_FILE_NAME;

use lsp_types::DidChangeTextDocumentParams;use lsp_types::request::Shutdown;
use lsp_types::Diagnostic;
use lsp_types::InitializeParams;
use lsp_types::InitializeResult;
use lsp_types::InitializedParams;
use lsp_types::PublishDiagnosticsParams;
use msg::Message;
use msg::Notification;
use msg::Request;
use msg::RequestId;
use msg::Response;
use serde::Deserialize;
use serde::Serialize;
use serde_json::json;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::io::Result as IoResult;
use std::result::Result as RustResult;
use std::str::Utf8Error;

use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;
use std::time::Instant;
use stdio::IoThreads;

use std::{
    collections::{HashMap, VecDeque},
    fmt::{format, Debug},
    io::{Read, Write},
    mem::MaybeUninit,
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
    sync::{Arc, Mutex, Once},
    thread::{self, JoinHandle, Thread},
};

#[derive(Debug)]
struct FileInfo {
    pub uri: String, // 文件名
    pub diagnostics: Vec<Diagnostic>,
}

impl FileInfo {
    pub fn new(uri: String) -> FileInfo {
        FileInfo {
            uri,
            diagnostics: Vec::new(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Response4E {
    pub code: i32, // 0: 成功返回，-1：server未返回，-9：其它错误
    pub msg: Option<Response>,
}

impl Response4E {
    pub fn new(code: i32, msg: Option<Response>) -> Response4E {
        Response4E { code, msg }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct LspServerInfo {
    pub name: String,
    pub version: String,
    pub id: String, // pid at the moment
    pub capabilities: String,
}

impl LspServerInfo {
    pub fn new(name: String, version: String, id: String, capabilities: String) -> LspServerInfo {
        LspServerInfo {
            name,
            version,
            id,
            capabilities,
        }
    }
}

const SERVER_STATUS_NEW: u8 = 0;
const SERVER_STATUS_STARTING: u8 = 1;
const SERVER_STATUS_RUNNING: u8 = 2;
const SERVER_STATUS_EXITING: u8 = 3;

struct LspServer {
    pub child: Option<Child>,
    pub server_info: LspServerInfo,
    pub status: u8, // 是否已经启动完 0：待启动，1：启动中，2：启动完成，3：退出中
    latest_request_id: Mutex<RequestId>,
    latest_response_id: Arc<Mutex<RequestId>>,
    transport: Arc<Mutex<Option<Connection>>>,
    transport_threads: Option<IoThreads>,
    dispatcher: Option<thread::JoinHandle<()>>,
    file_infos: Arc<Mutex<HashMap<String, FileInfo>>>, // key: uri
    requests: Arc<Mutex<VecDeque<Request>>>,           // server发送的最新请求，只存最新的
    responses: Arc<Mutex<VecDeque<Response>>>,         // server发送的最新响应，只存最新的
    notifications: Arc<Mutex<VecDeque<Notification>>>, // server发送的最新通知（非诊断），只存最新的
    exit: Arc<Mutex<bool>>,
}

impl LspServer {
    pub fn new(
        root_uri: String,
        cmd: String,
        cmd_args: String,
        initialize_req: String,
    ) -> Option<LspServer> {
        let args = cmd_args.split_ascii_whitespace().collect::<Vec<&str>>();

        let mut child = Command::new(cmd)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn();

        if let Ok(mut c) = child {
            let mut server = LspServer {
                child: None,
                server_info: LspServerInfo::new(
                    "".to_string(),
                    "".to_string(),
                    "".to_string(),
                    "".to_string(),
                ),
                status: SERVER_STATUS_NEW,
                latest_request_id: Mutex::new(RequestId::from(-1)),
                latest_response_id: Arc::new(Mutex::new(RequestId::from(-1))),
                transport: Arc::new(Mutex::new(None)),
                transport_threads: None,
                dispatcher: None,
                file_infos: Arc::new(Mutex::new(HashMap::new())),
                requests: Arc::new(Mutex::new(VecDeque::new())),
                responses: Arc::new(Mutex::new(VecDeque::new())),
                notifications: Arc::new(Mutex::new(VecDeque::new())),
                exit: Arc::new(Mutex::new(false)),
            };

            let mut stdin = c.stdin.take().unwrap();
            let mut stdout = c.stdout.take().unwrap();

            let (mut transport, mut transport_threads) = Connection::stdio(stdin, stdout);

            server.server_info.id = c.id().to_string();
            server.child = Some(c);
            server.transport = Arc::new(Mutex::new(Some(transport)));
            server.file_infos = Arc::new(Mutex::new(HashMap::new()));
            server.transport_threads = Some(transport_threads);
            server.dispatcher = Some(LspServer::start_dispatcher(
                Arc::clone(&server.transport),
                Arc::clone(&server.exit),
                Arc::clone(&server.requests),
                Arc::clone(&server.responses),
                Arc::clone(&server.notifications),
                Arc::clone(&server.file_infos),
                Arc::clone(&server.latest_response_id),
            ));
            server.status = SERVER_STATUS_STARTING;

            Some(server)
        } else {
            Logger::log(&format!(
                "create child process failed with error {:?}",
                child.err().unwrap(),
            ));
            None
        }
    }

    fn start_dispatcher(
        transport2: Arc<Mutex<Option<Connection>>>,
        exit2: Arc<Mutex<bool>>,
        requests2: Arc<Mutex<VecDeque<Request>>>,
        responses2: Arc<Mutex<VecDeque<Response>>>,
        notifications2: Arc<Mutex<VecDeque<Notification>>>,
        file_infos2: Arc<Mutex<HashMap<String, FileInfo>>>,
        latest_response_id2: Arc<Mutex<RequestId>>,
    ) -> thread::JoinHandle<()> {
        let handle = thread::spawn(move || loop {
            {
                let exit = exit2.lock().unwrap();
                if *exit {
                    break;
                }
            }

            let mut message: Option<Message> = None;
            {
                let transport = transport2.lock().unwrap();
                message = transport.as_ref().unwrap().read();
            }

            if let Some(m) = message {
                match m {
                    Message::Request(r) => {
                        // TODO save request into the queue
                    }
                    Message::Response(r) => {
                        let id = r.id.clone();
                        {
                            let mut responses = responses2.lock().unwrap();
                            responses.push_back(r);
                        }
                        {
                            let mut lrid = latest_response_id2.lock().unwrap();
                            if lrid.lt(&id) {
                                *lrid = id;
                            }
                            Logger::log(&format!("update latest_response_id to {}", *lrid));
                        }
                    }
                    Message::Notification(r) => {
                        if r.method.eq("textDocument/publishDiagnostics") {
                            let params =
                                serde_json::from_value::<PublishDiagnosticsParams>(r.params)
                                    .unwrap();

                            let uri = params.uri.as_str().to_string();
                            let mut file_info = FileInfo::new(uri.clone());
                            file_info.diagnostics = params.diagnostics;

                            let mut file_infos = file_infos2.lock().unwrap();
                            file_infos.insert(uri.clone(), file_info);
                        } else {
                            // other notifications NOTE
                        }
                    }
                }
            } else {
                thread::sleep(std::time::Duration::from_millis(10));
            }
        });

        return handle;
    }

    pub fn stop_dispatcher(&mut self) {
        let mut exit = self.exit.lock().unwrap();
        *exit = true;
    }

    pub fn kill_child(&mut self) {
        if self.child.is_some() {
            self.child.take().unwrap().kill();
            self.child = None;
        }
    }

    pub fn update_latest_request_id(&self, id: RequestId) {
        let mut latest_request_id = self.latest_request_id.lock().unwrap();
        if *latest_request_id < id {
            *latest_request_id = id;
        }
    }

    pub fn get_latest_response_id(&self) -> RequestId {
        let lrid = self.latest_response_id.lock().unwrap();

        lrid.clone()
    }

    pub fn exit_transport(&self) {
        let transport = self.transport.lock().unwrap();
        if transport.is_some() {
            transport.as_ref().unwrap().to_exit();
        }
    }

    pub fn write(&self, request: Message) -> RustResult<(), LspceError> {
        let transport = self.transport.lock().unwrap();
        if transport.is_some() {
            let result = transport.as_ref().unwrap().write(request);
            match result {
                Ok(_) => Ok(()),
                Err(e) => Err(LspceError(e.to_string())),
            }
        } else {
            Err(LspceError("transport is not established.".to_string()))
        }
    }

    pub fn read_response(&self) -> Option<Response> {
        let mut responses = self.responses.lock().unwrap();

        responses.pop_front()
    }

    //
    pub fn read_response_exact(&self, id: RequestId, method: String) -> Option<Response> {
        let mut responses = self.responses.lock().unwrap();
        let mut result: Option<Response>;
        loop {
            let resp = responses.pop_front();
            if resp.is_some() {
                let resp = resp.unwrap();

                Logger::log(&format!(
                    "read_response_exact request_id {}, method {}, get {}",
                    id,
                    method,
                    serde_json::to_string_pretty(&resp).unwrap_or("invalid json".to_string())
                ));

                if id.eq(&resp.id) {
                    result = Some(resp);
                    break;
                } else if id.lt(&resp.id) {
                    responses.push_front(resp);
                    result = None;
                    break;
                }
            } else {
                Logger::log(&format!(
                    "read_response_exact get null for request_id {}, method {}",
                    id, method
                ));
                result = None;
                break;
            }
        }
        result
    }

    pub fn read_notification(&self) -> Option<Notification> {
        let mut notifications = self.notifications.lock().unwrap();

        notifications.pop_front()
    }

    pub fn read_request(&self) -> Option<Request> {
        let mut requests = self.requests.lock().unwrap();

        requests.pop_front()
    }

    pub fn clear_diagnostics(&self, uri: &str) {
        let mut file_infos = self.file_infos.lock().unwrap();
        if let Some(mut file_info) = file_infos.get_mut(uri) {
            let result = serde_json::to_string(&file_info.diagnostics);
            file_info.diagnostics = Vec::new();
        }
    }
}

struct Project {
    pub root_uri: String,                    // 项目根目录
    pub servers: HashMap<String, LspServer>, // 每个LspServer处理一种类型的文件
}

impl Project {
    pub fn new(root_uri: String) -> Project {
        Project {
            root_uri,
            servers: HashMap::new(),
        }
    }
}

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name("lspce-module"))]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done loading!")
}

/// disable logging to /tmp/lspce.log
#[defun]
fn disable_logging(env: &Env) -> Result<Value<'_>> {
    LOG_ENABLE.store(0, Ordering::Relaxed);

    env.message("Logging is disabled!")
}

/// enable logging to /tmp/lspce.log
#[defun]
fn enable_logging(env: &Env) -> Result<Value<'_>> {
    LOG_ENABLE.store(1, Ordering::Relaxed);

    env.message("Logging is enabled!")
}

/// set logging file name
#[defun]
fn set_log_file(env: &Env, file: String) -> Result<Value<'_>> {
    *LOG_FILE_NAME.lock().unwrap() = file.clone();

    env.message(&format!("Set logging file to {}", file))
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

/// Connect to an existing server or create a server subprocess and then connect to it.
#[defun]
fn connect(
    env: &Env,
    root_uri: String,
    lsp_type: String,
    cmd: String,
    cmd_args: String,
    initialize_req: String,
    timeout: i32,
) -> Result<Option<String>> {
    Logger::log(&format!(
        "start initializing server for lsp_type {} in project {}",
        lsp_type, root_uri
    ));

    let mut projects = projects().lock().unwrap();

    if let Some(p) = projects.get(&root_uri) {
        if let Some(s) = p.servers.get(&lsp_type) {
            Logger::log(&format!(
                "server created already for lsp_type {} in project {}",
                lsp_type, root_uri
            ));

            return Ok(Some(serde_json::to_string(&s.server_info).unwrap()));
        }
    }

    let mut server = LspServer::new(
        root_uri.clone(),
        cmd.clone(),
        cmd_args.clone(),
        initialize_req.clone(),
    );
    if let Some(mut s) = server {
        let server_info: LspServerInfo;

        let mut project = projects.get_mut(&root_uri);
        if let Some(p) = project.as_mut() {
            if initialize(env, root_uri.clone(), &mut s, initialize_req, timeout) {
                server_info = s.server_info.clone();
                p.servers.insert(lsp_type, s);
            } else {
                s.kill_child();
                return Ok(None);
            }
        } else {
            let mut proj = Project::new(root_uri.clone());
            if initialize(env, root_uri.clone(), &mut s, initialize_req, timeout) {
                server_info = s.server_info.clone();
                proj.servers.insert(lsp_type, s);

                projects.insert(root_uri.clone(), proj);
            } else {
                s.kill_child();
                return Ok(None);
            }
        }

        Logger::log(&format!("Connected to server successfully."));
        return Ok(Some(serde_json::to_string(&server_info).unwrap()));
    } else {
        Logger::log(&format!("Failed to connect to server."));
        return Ok(None);
    }
}

fn initialize(
    env: &Env,
    root_uri: String,
    server: &mut LspServer,
    req_str: String,
    timeout: i32,
) -> bool {
    Logger::log(&format!("initialize request {:#?}", req_str));

    let msg = serde_json::from_str::<Request>(&req_str);
    if msg.is_err() {
        Logger::log(&format!("request is not valid json {}", &req_str));
        return false;
    }

    let msg = msg.unwrap();
    let id = msg.id.clone();

    if !_request_async(server, msg) {
        return false;
    }

    let start_time = Instant::now();
    loop {
        let response = server.read_response();
        match response {
            Some(m) => {
                let r_id = m.id.clone();
                if r_id.eq(&id) {
                    if m.error.is_some() {
                        // 有错误
                        env.message(&format!("Lsp error {:?}", m.error));
                        return false;
                    }

                    if let Ok(ir) = serde_json::from_value::<InitializeResult>(m.result.unwrap()) {
                        let initialized = Notification::new(
                            "initialized".to_string(),
                            serde_json::to_value(InitializedParams {}).unwrap(),
                        );

                        _notify(env, server, initialized);

                        // 记录server初始化完成
                        server.status = SERVER_STATUS_RUNNING;

                        if let Some(si) = ir.server_info {
                            server.server_info.name = si.name.clone();
                            server.server_info.version = si.version.expect("");
                        }
                        server.server_info.capabilities =
                            serde_json::to_string(&ir.capabilities).unwrap();

                        return true;
                    } else {
                        return false;
                    }
                } else {
                    thread::sleep(std::time::Duration::from_millis(100));
                }
            }
            None => {
                thread::sleep(std::time::Duration::from_millis(100));
            }
        }
        if timeout > 0
            && Instant::now().duration_since(start_time).as_millis() > timeout as u128 * 1000
        {
            env.message(&format!("timeout when initializing server."));
            Logger::log(&format!("timeout when initializing server."));

            return false;
        }
    }
}

#[defun]
fn shutdown(env: &Env, root_uri: String, file_type: String, req: String) -> Result<Option<String>> {
    let mut projects = projects().lock().unwrap();
    if let Some(mut p) = projects.get_mut(&root_uri) {
        if let Some(mut server) = p.servers.get_mut(&file_type) {
            // 退出中，不再向server发送请求和通知。
            server.status = SERVER_STATUS_EXITING;

            let msg = serde_json::from_str::<Request>(&req);
            if msg.is_err() {
                Logger::log(&format!("request is not valid json {}", &req));
                return Ok(None);
            }

            let msg = msg.unwrap();
            let id = msg.id.clone();

            if !_request_async(server, msg) {
                return Ok(None);
            }

            let start_time = Instant::now();
            loop {
                let response = server.read_response();
                match response {
                    Some(r) => {
                        let r_id = r.id.clone();
                        if r_id.eq(&id) {
                            let exit = Notification::new("exit".to_string(), json!({}));

                            _notify(env, server, exit);

                            server.stop_dispatcher();
                            server.exit_transport();
                            Logger::log("after exit transport.");
                            // 等待读写线程结束
                            server.transport_threads.take().unwrap().join();
                            Logger::log("after thread join.");
                            // 等待子进程结束，否则会成僵尸进程。
                            server.child.take().unwrap().wait();
                            Logger::log("after child wait.");

                            p.servers.remove(&file_type);
                            if p.servers.len() == 0 {
                                projects.remove(&root_uri);
                            }

                            return Ok(Some(serde_json::to_string(&r).unwrap()));
                        }
                    }
                    None => {
                        thread::sleep(std::time::Duration::from_millis(100));
                    }
                }

                if Instant::now().duration_since(start_time).as_millis() > 3 * 1000 {
                    env.message(&format!("timeout when shutdown server"));

                    server.stop_dispatcher();
                    server.exit_transport();
                    server.kill_child();

                    p.servers.remove(&file_type);
                    if p.servers.len() == 0 {
                        projects.remove(&root_uri);
                    }

                    return Ok(None);
                }
            }
        } else {
            env.message(&format!("No server for {}", &file_type));

            return Ok(None);
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));

        return Ok(None);
    }

    Ok(None)
}

#[defun]
fn server(env: &Env, root_uri: String, file_type: String) -> Result<Option<String>> {
    let projects = projects().lock().unwrap();

    if let Some(p) = projects.get(&root_uri) {
        if let Some(s) = p.servers.get(&file_type) {
            return Ok(Some(serde_json::to_string(&s.server_info).unwrap()));
        }
    }

    Ok(None)
}

fn _request_async(server: &mut LspServer, req: Request) -> bool {
    let method = req.method.clone();
    let id = req.id.clone();

    // 更新最新的请求id
    server.update_latest_request_id(id.clone());

    if method == "textDocument/didChange" || method == "textDocument/didClose"{
        let param = serde_json::from_value::<DidChangeTextDocumentParams>(req.params.clone());
        if let Ok(param) = param {
            server.clear_diagnostics(&param.text_document.uri.to_string());
        }
    }
    

    let write_result = server.write(Message::Request(req));
    match write_result {
        Ok(_) => return true,
        Err(e) => {
            Logger::log(&format!("request error {:#?}", e));

            return false;
        }
    }
}

#[defun]
fn request_async(
    env: &Env,
    root_uri: String,
    file_type: String,
    req: String,
) -> Result<Option<bool>> {
    let mut projects = projects().lock().unwrap();
    if let Some(mut p) = projects.get_mut(&root_uri) {
        if let Some(mut server) = p.servers.get_mut(&file_type) {
            // 检查server是否初始化完成
            if server.status != SERVER_STATUS_RUNNING {
                return Ok(None);
            }

            let msg = serde_json::from_str::<Request>(&req);
            if msg.is_err() {
                Logger::log(&format!("request is not valid json {}", &req));
                return Ok(None);
            }
            let msg = msg.unwrap();

            if _request_async(server, msg) {
                return Ok(Some(true));
            } else {
                return Ok(None);
            }
        } else {
            env.message(&format!("No server for {}", &file_type));

            return Ok(None);
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));

        return Ok(None);
    }
}

fn _notify(env: &Env, server: &mut LspServer, req: Notification) -> Result<Option<bool>> {
    let method = req.method.clone();

    let write_result = server.write(Message::Notification(req));

    match write_result {
        Ok(_) => {
            return Ok(Some(true));
        }
        Err(e) => {
            Logger::log(&format!("notify error {:#?}", e));
            return Ok(None);
        }
    }

    return Ok(None);
}

#[defun]
fn notify(env: &Env, root_uri: String, file_type: String, req: String) -> Result<Option<bool>> {
    let mut projects = projects().lock().unwrap();
    if let Some(mut p) = projects.get_mut(&root_uri) {
        if let Some(mut server) = p.servers.get_mut(&file_type) {
            // 检查server是否初始化完成
            if server.status != SERVER_STATUS_RUNNING {
                env.message(&format!("Server is not ready."));
                return Ok(None);
            }

            let json_object = serde_json::from_str::<Notification>(&req);
            match json_object {
                Ok(notification) => {
                    return _notify(env, server, notification);
                }
                Err(e) => {
                    env.message(&format!("Invalid json string {}", &req));
                }
            }
        } else {
            env.message(&format!("No server for {}", &file_type));
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));
    }

    return Ok(None);
}

/// precondition: have called read_latest_response_id and gotten the id.
#[defun]
fn read_response_exact(
    env: &Env,
    root_uri: String,
    file_type: String,
    id: i32,
    method: String,
) -> Result<Option<String>> {
    let req_id = RequestId::from(id);

    let projects = projects().lock().unwrap();
    if let Some(p) = projects.get(&root_uri) {
        if let Some(server) = p.servers.get(&file_type) {
            let lrid = server.latest_response_id.lock().unwrap();
            if lrid.lt(&req_id) {
                return Ok(None);
            }

            let response = server.read_response_exact(req_id, method);
            match response {
                Some(r) => {
                    return Ok(Some(r.str));
                }
                None => {
                    return Ok(None);
                }
            }
        } else {
            env.message(&format!("No server for {}", &file_type));
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));
    }

    Ok(None)
}

#[defun]
fn read_file_diagnostics(
    env: &Env,
    root_uri: String,
    file_type: String,
    uri: String,
) -> Result<Option<String>> {
    let projects = projects().lock().unwrap();
    if let Some(p) = projects.get(&root_uri) {
        if let Some(server) = p.servers.get(&file_type) {
            let mut file_infos = server.file_infos.lock().unwrap();
            if let Some(file_info) = file_infos.get_mut(&uri) {
                let result = serde_json::to_string(&file_info.diagnostics);

                if result.is_ok() {
                    return Ok(Some(result.unwrap()));
                }
            }
        } else {
            env.message(&format!("No server for {}", &file_type));
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));
    }

    Ok(None)
}

#[defun]
fn read_latest_response_id(
    env: &Env,
    root_uri: String,
    file_type: String,
) -> Result<Option<String>> {
    let projects = projects().lock().unwrap();
    if let Some(p) = projects.get(&root_uri) {
        if let Some(server) = p.servers.get(&file_type) {
            let lrid = server.latest_response_id.lock().unwrap();

            return Ok(Some(lrid.to_string()));
        } else {
            env.message(&format!("No server for {}", &file_type));
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));
    }

    Ok(None)
}
