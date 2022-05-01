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
use lsp_types::InitializeParams;
use lsp_types::InitializedParams;
use msg::Message;
use msg::Notification;
use msg::Request;
use msg::RequestId;
use serde_json::json;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::fs::File;
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

struct LspServer {
    pub child: Option<Child>,
    pub nick_name: String,
    pub initialized: bool,               // 是否已经启动完
    pub capabilities: String,            // TODO 类型
    pub usable_capabilites: String,      // TODO
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
        lsp_args: String,
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
                nick_name: "".to_string(),
                initialized: false,
                capabilities: "".to_string(),
                usable_capabilites: "".to_string(),
                latest_id: Mutex::new(RequestId::from(-1)),
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

    pub fn read(&self) -> Option<Message> {
        if self.transport.is_some() {
            self.transport.as_ref().unwrap().read()
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
    lsp_args: String,
) -> Result<String> {
    Logger::log(&format!(
        "start initializing server for file_type {} in project {}",
        file_type, root_uri
    ));
    if (server_running(env, root_uri.clone(), file_type.clone()).unwrap()) {
        Logger::log(&format!(
            "server created already for file_type {} in project {}",
            file_type, root_uri
        ));
        return Ok("server created already, return now.".to_string());
    }

    let mut server = LspServer::new(
        root_uri.clone(),
        cmd.clone(),
        cmd_args.clone(),
        lsp_args.clone(),
    );
    if let Some(mut s) = server {
        let mut projects = projects().lock().unwrap();
        let mut project = projects.get_mut(&root_uri);
        if let Some(p) = project.as_mut() {
            initialize(env, root_uri.clone(), &mut s, lsp_args);

            p.servers.insert(file_type, s);
        } else {
            let mut proj = Project::new(root_uri.clone(), "".to_string());
            initialize(env, root_uri.clone(), &mut s, lsp_args);

            proj.servers.insert(file_type, s);

            projects.insert(root_uri.clone(), proj);
        }

        Logger::log(&format!("connected to server."));
    } else {
        Logger::log(&format!("connect failed"));
    }

    Ok("server created".to_string())
}

fn initialize(
    env: &Env,
    root_uri: String,
    server: &mut LspServer,
    lsp_args: String,
) -> Result<Option<String>> {
    Logger::log(&format!("initialize request {:#?}", lsp_args));

    let resp_value = _request(env, server, lsp_args);

    match resp_value {
        Ok(resp) => match resp {
            Some(m) => {
                let n: InitializedParams = InitializedParams {};
                let initialized: Notification = Notification {
                    method: "initialized".to_string(),
                    params: serde_json::to_value(n).unwrap(),
                };

                _notify2(env, server, initialized);

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

    Ok(Some("initialized".to_string()))
}

// 返回server信息 TODO
#[defun]
fn server(env: &Env, root_uri: String, file_type: String) -> Result<bool> {
    let projects = projects().lock().unwrap();

    Ok(false)
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

fn _server(root_uri: String, file_type: String) -> Option<&'static LspServer> {
    let projects = projects().lock().unwrap();

    None
}

fn _request(env: &Env, server: &mut LspServer, req: String) -> Result<Option<Message>> {
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
                    let read_value = server.read();
                    Logger::log(&format!("initialize ok {:#?}", read_value));

                    match read_value {
                        Some(m) => match m {
                            Message::Request(r) => {
                                Logger::log(&format!("request from server, content is {:?}", r));
                                todo!()
                            }
                            Message::Notification(n) => {
                                Logger::log(&format!(
                                    "notification from server, content is {:?}",
                                    n
                                ));
                                todo!()
                            }
                            Message::Response(r) => {
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
                                        return Ok(Some(Message::Response(r)));
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
                        },
                        None => {
                            //nothing
                            continue;
                        }
                    }
                    thread::sleep(std::time::Duration::from_millis(100));
                }
            },
            Err(e) => {
                env.message(&format!("initialize error {:#?}", e));

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

fn _notify(env: &Env, server: &mut LspServer, req: String) -> Result<Option<bool>> {
    if let Ok(msg) = serde_json::from_str::<Notification>(&req) {
        let write_result = server.write(Message::Notification(msg));

        match write_result {
            Ok(_) => {
                Logger::log(&format!("notify successfully: {:#?}", &req));
                return Ok(Some(true));
            }
            Err(e) => {
                Logger::log(&format!("notify error {:#?}", e));
                return Ok(None);
            }
        }
    } else {
        env.message(&format!("request is not valid json {}", &req));

        return Ok(None);
    };

    return Ok(None);
}

fn _notify2(env: &Env, server: &mut LspServer, req: Notification) -> Result<Option<bool>> {
    let method = req.method.clone();

    let write_result = server.write(Message::Notification(req));

    match write_result {
        Ok(_) => {
            Logger::log(&format!("notify successfully: {}", method));
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
            return _notify(env, server, req);
        } else {
            env.message(&format!("No server for {}", &file_type));

            return Ok(None);
        }
    } else {
        env.message(&format!("No project for {} {}", &root_uri, &file_type));

        return Ok(None);
    }
}

#[defun]
fn notify2(env: &Env, root_uri: String, file_type: String, req: String) -> Result<Option<bool>> {
    let mut projects = projects().lock().unwrap();
    if let Some(mut p) = projects.get_mut(&root_uri) {
        if let Some(mut server) = p.servers.get_mut(&file_type) {
            let json_object = serde_json::from_str::<Notification>(&req);
            match json_object {
                Ok(notification) => {
                    return _notify2(env, server, notification);
                }
                Err(e) => {
                    env.message(&format!("Invalid json string {}", &req));

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
