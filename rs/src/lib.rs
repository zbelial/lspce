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
use lsp_types::request::Shutdown;
use lsp_types::InitializeParams;
use lsp_types::InitializeResult;
use lsp_types::InitializedParams;
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
use std::ptr::read_volatile;
use std::result::Result as RustResult;
use std::str::Utf8Error;
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

struct LspServer {
    pub child: Option<Child>,
    pub server_info: LspServerInfo,
    pub initialized: u8, // 是否已经启动完 0：待启动，1：启动中，2：启动完成
    pub uris: HashMap<String, FileInfo>, // key: uri
    latest_id: Mutex<RequestId>,
    transport: Option<Connection>,
    threads: Option<IoThreads>,
}

impl LspServer {
    pub fn new(
        root_uri: String,
        cmd: String,
        cmd_args: String,
        initialize_params: String,
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
                initialized: 0,
                latest_id: Mutex::new(RequestId::from(-1)),
                uris: HashMap::new(),
                transport: None,
                threads: None,
            };

            let mut stdin = c.stdin.take().unwrap();
            let mut stdout = c.stdout.take().unwrap();

            let (mut transport, mut threads) = Connection::stdio(stdin, stdout);

            server.server_info.id = c.id().to_string();
            server.child = Some(c);
            server.transport = Some(transport);
            server.threads = Some(threads);

            Some(server)
        } else {
            None
        }
    }

    pub fn update_latest_id(&self, id: RequestId) {
        let mut latest_id = self.latest_id.lock().unwrap();
        if *latest_id < id {
            *latest_id = id;
        }
    }

    pub fn is_outdate(&self, id: RequestId) -> bool {
        let latest_id = self.latest_id.lock().unwrap();

        return id < *latest_id;
    }

    pub fn exit_transport(&self) {
        if self.transport.is_some() {
            self.transport.as_ref().unwrap().to_exit();
        }
    }

    pub fn write(&self, request: Message) -> RustResult<(), LspceError> {
        if self.transport.is_some() {
            let result = self.transport.as_ref().unwrap().write(request);
            match result {
                Ok(_) => Ok(()),
                Err(e) => Err(LspceError(e.to_string())),
            }
        } else {
            Err(LspceError("transport is not established.".to_string()))
        }
    }

    pub fn read_response(&self) -> Option<Response> {
        if self.transport.is_some() {
            self.transport.as_ref().unwrap().read_response()
        } else {
            None
        }
    }

    pub fn read_notification(&self) -> Option<Notification> {
        if self.transport.is_some() {
            self.transport.as_ref().unwrap().read_notification()
        } else {
            None
        }
    }

    pub fn read_request(&self) -> Option<Request> {
        if self.transport.is_some() {
            self.transport.as_ref().unwrap().read_request()
        } else {
            None
        }
    }
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

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name("lspce-module"))]
fn init(env: &Env) -> Result<Value<'_>> {
    Logger::log("Done loading!");

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

#[defun]
fn connect(
    env: &Env,
    root_uri: String,
    file_type: String,
    cmd: String,
    cmd_args: String,
    initialize_params: String,
) -> Result<Option<String>> {
    Logger::log(&format!(
        "start initializing server for file_type {} in project {}",
        file_type, root_uri
    ));

    let mut projects = projects().lock().unwrap();

    if let Some(p) = projects.get(&root_uri) {
        if let Some(s) = p.servers.get(&file_type) {
            Logger::log(&format!(
                "server created already for file_type {} in project {}",
                file_type, root_uri
            ));

            return Ok(Some("server created already, return now.".to_string()));
        }
    }

    let mut server = LspServer::new(
        root_uri.clone(),
        cmd.clone(),
        cmd_args.clone(),
        initialize_params.clone(),
    );
    if let Some(mut s) = server {
        let server_info: LspServerInfo;
        let mut project = projects.get_mut(&root_uri);
        if let Some(p) = project.as_mut() {
            if let Some(_) = initialize(env, root_uri.clone(), &mut s, initialize_params) {
                server_info = s.server_info.clone();
                p.servers.insert(file_type, s);
            } else {
                return Ok(None);
            }
        } else {
            let mut proj = Project::new(root_uri.clone(), "".to_string());
            if let Some(_) = initialize(env, root_uri.clone(), &mut s, initialize_params) {
                server_info = s.server_info.clone();
                proj.servers.insert(file_type, s);

                projects.insert(root_uri.clone(), proj);
            } else {
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
    initialize_params: String,
) -> Option<bool> {
    Logger::log(&format!("initialize request {:#?}", initialize_params));

    let resp_value = _request(env, server, initialize_params);

    match resp_value {
        Ok(resp) => match resp {
            Some(m) => {
                if m.error.is_some() {
                    // 有错误
                    env.message(&format!("Lsp error {:?}", m.error));
                    return None;
                }

                if let Ok(ir) = serde_json::from_value::<InitializeResult>(m.result.unwrap()) {
                    let n: InitializedParams = InitializedParams {};
                    let initialized: Notification = Notification {
                        method: "initialized".to_string(),
                        params: serde_json::to_value(n).unwrap(),
                    };

                    _notify(env, server, initialized);

                    // TODO 记录server初始化完成
                    if let Some(si) = ir.server_info {
                        server.server_info.name = si.name.clone();
                        server.server_info.version = si.version.expect("");
                    }
                    server.server_info.capabilities =
                        serde_json::to_string(&ir.capabilities).unwrap();

                    return Some(true);
                } else {
                    return None;
                }
            }
            None => {
                return None;
            }
        },
        Err(e) => {
            env.message(&format!("Response error {}", e.to_string()));
            return None;
        }
    }
}

#[defun]
fn shutdown(
    env: &Env,
    root_uri: String,
    file_type: String,
    shutdown: String,
) -> Result<Option<String>> {
    let mut projects = projects().lock().unwrap();
    if let Some(mut p) = projects.get_mut(&root_uri) {
        if let Some(mut server) = p.servers.get_mut(&file_type) {
            let resp_value = _request(env, server, shutdown);

            match resp_value {
                Ok(resp) => match resp {
                    Some(m) => {
                        let exit: Notification = Notification {
                            method: "exit".to_string(),
                            params: json!({}),
                        };

                        // FIXME 同时有多个lsp server子进程时，可能会卡住。线程里结束子进程？
                        _notify(env, server, exit);

                        server.exit_transport();
                        Logger::log("after exit transport.");
                        // 等待读写线程结束
                        server.threads.take().unwrap().join();
                        Logger::log("after thread join.");
                        // 等待子进程结束，否则会成僵尸进程。
                        server.child.take().unwrap().wait();
                        Logger::log("after child wait.");

                        p.servers.remove(&file_type);
                        if p.servers.len() == 0 {
                            projects.remove(&root_uri);
                        }

                        return Ok(Some(serde_json::to_string(&m).unwrap()));
                    }
                    None => {
                        return Ok(None);
                    }
                },
                Err(e) => {
                    env.message(&format!("Response error {}", e.to_string()));
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

fn _server(root_uri: String, file_type: String) -> Option<&'static LspServer> {
    let projects = projects().lock().unwrap();

    None
}

fn _request(env: &Env, server: &mut LspServer, req: String) -> Result<Option<Response>> {
    if let Ok(msg) = serde_json::from_str::<Request>(&req) {
        let start_time = Instant::now();
        let method = msg.method.clone();
        let id = msg.id.clone();

        // 更新最新的请求id，用于判断response是否过时。
        server.update_latest_id(id.clone());

        let write_result = server.write(Message::Request(msg));

        match write_result {
            Ok(_) => loop {
                if Instant::now().duration_since(start_time).as_millis() > 2000 {
                    env.message(&format!("timeout in {}", method));

                    return Ok(None);
                } else {
                    let read_value = server.read_response();

                    match read_value {
                        Some(r) => {
                            // Logger::log(&format!("request {} ok {:#?}", &method, &r));

                            let ret_id = r.id.clone();
                            let latest_id = server.latest_id.lock().unwrap();
                            if id == *latest_id {
                                if ret_id < id {
                                    //nothing
                                    Logger::log(&format!(
                                        "outdated response for id {}",
                                        ret_id.to_string()
                                    ));
                                    continue;
                                } else if ret_id == id {
                                    return Ok(Some(r));
                                } else {
                                    // ret_id > id
                                    // 有更新的响应，当前请求可以丢弃了。
                                    Logger::log(&format!(
                                        "response for newer reqeust id {}",
                                        id.clone()
                                    ));
                                    return Ok(None);
                                }
                            } else {
                                Logger::log(&format!(
                                    "current request is outdated, id {}, latest_id {}",
                                    id, *latest_id
                                ));

                                return Ok(None);
                            }
                        }
                        None => {
                            //nothing
                            continue;
                        }
                    }
                    thread::sleep(std::time::Duration::from_millis(100));
                }
            },
            Err(e) => {
                env.message(&format!("request error {:#?}", e));

                return Ok(None);
            }
        }
    } else {
        env.message(&format!("request is not valid json {}", &req));

        return Ok(None);
    };

    return Ok(None);
}

#[defun]
fn request(env: &Env, root_uri: String, file_type: String, req: String) -> Result<Option<String>> {
    let mut projects = projects().lock().unwrap();
    if let Some(mut p) = projects.get_mut(&root_uri) {
        if let Some(mut server) = p.servers.get_mut(&file_type) {
            // TODO 检查server是否初始化完成

            let resp_value = _request(env, server, req);
            match resp_value {
                Ok(resp) => match resp {
                    Some(m) => {
                        return Ok(Some(serde_json::to_string(&m).unwrap()));
                    }
                    None => {
                        return Ok(None);
                    }
                },
                Err(e) => {
                    env.message(&format!("error response {}", e.to_string()));
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
}

fn _notify(env: &Env, server: &mut LspServer, req: Notification) -> Result<bool> {
    let method = req.method.clone();

    let write_result = server.write(Message::Notification(req));

    match write_result {
        Ok(_) => {
            Logger::log(&format!("notify successfully: {}", method));
            return Ok(true);
        }
        Err(e) => {
            Logger::log(&format!("notify error {:#?}", e));
            return Ok(false);
        }
    }

    return Ok(false);
}

#[defun]
fn notify(env: &Env, root_uri: String, file_type: String, req: String) -> Result<bool> {
    let mut projects = projects().lock().unwrap();
    if let Some(mut p) = projects.get_mut(&root_uri) {
        if let Some(mut server) = p.servers.get_mut(&file_type) {
            // TODO 检查server是否初始化完成

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

    return Ok(false);
}

#[defun]
fn read_notifications(
    env: &Env,
    root_uri: String,
    file_type: String,
    req: String,
) -> Result<Option<String>> {
    let mut notifications: Vec<Notification> = vec![];

    let projects = projects().lock().unwrap();
    if let Some(p) = projects.get(&root_uri) {
        if let Some(server) = p.servers.get(&file_type) {
            // TODO 检查server是否初始化完成

            loop {
                let read_value = server.read_notification();
                match read_value {
                    Some(r) => {
                        notifications.push(r);
                    }
                    None => {
                        break;
                    }
                }
            }
        } else {
            env.message(&format!("No server for {}", &file_type));
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));
    }

    Ok(Some(serde_json::to_string(&notifications).unwrap()))
}

#[defun]
fn read_request(
    env: &Env,
    root_uri: String,
    file_type: String,
    req: String,
) -> Result<Option<String>> {
    let projects = projects().lock().unwrap();
    if let Some(p) = projects.get(&root_uri) {
        if let Some(server) = p.servers.get(&file_type) {
            // TODO 检查server是否初始化完成

            if let Some(r) = server.read_request() {
                return Ok(Some(serde_json::to_string(&r).unwrap()));
            }
        } else {
            env.message(&format!("No server for {}", &file_type));
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));
    }

    Ok(None)
}
