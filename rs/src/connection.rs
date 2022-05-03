//! steal from https://docs.rs/lsp-server/latest/lsp_server/
//! A language server scaffold, exposing a synchronous crossbeam-channel based API.
//! This crate handles protocol handshaking and parsing messages, while you
//! control the message dispatch loop yourself.
//!
//! Run with `RUST_LOG=lsp_server=debug` to see all the messages.

use std::{
    collections::VecDeque,
    io,
    net::{TcpListener, TcpStream, ToSocketAddrs},
    process::{ChildStdin, ChildStdout},
    sync::{Arc, Mutex},
};

use crossbeam_channel::{Receiver, SendError, Sender};

use crate::{
    error::{ExtractError, ProtocolError},
    logger::Logger,
    msg::{ErrorCode, Message, Notification, Request, RequestId, Response, ResponseError},
    stdio::IoThreads,
};
use crate::{socket, stdio};

/// Connection is just a pair of channels of LSP messages.
pub struct Connection {
    sender: Sender<Message>,
    receiver: Receiver<Message>,
    responses: Arc<Mutex<VecDeque<Response>>>,
    notifications: Arc<Mutex<VecDeque<Notification>>>,
    requests: Arc<Mutex<VecDeque<Request>>>,
    exit_reader: Arc<Mutex<bool>>,
    exit_writer: Arc<Mutex<bool>>,
}

impl Connection {
    pub fn write(&self, req: Message) -> Result<(), SendError<Message>> {
        Logger::log(&format!("Connection write {:#?}", &req));

        self.sender.send(req)
    }

    pub fn read_response(&self) -> Option<Response> {
        let msg = self.responses.lock().unwrap().pop_front();
        Logger::log(&format!("Connection read response {:#?}", &msg));

        msg
    }

    pub fn read_notification(&self) -> Option<Notification> {
        let msg = self.notifications.lock().unwrap().pop_front();
        Logger::log(&format!("Connection read notification {:#?}", &msg));

        msg
    }

    pub fn read_request(&self) -> Option<Request> {
        let msg = self.requests.lock().unwrap().pop_front();
        Logger::log(&format!("Connection read request {:#?}", &msg));

        msg
    }

    pub fn to_exit(&self) {
        let mut exit_reader = self.exit_reader.lock().unwrap();
        *exit_reader = true;

        Logger::log("after exit_reader");

        let mut exit_writer = self.exit_writer.lock().unwrap();
        *exit_writer = true;

        // drop(&self.sender);
        Logger::log("after exit_writer");
    }

    /// Create connection over standard in/standard out.
    ///
    /// Use this to create a real language server.
    pub fn stdio(mut stdin: ChildStdin, mut stdout: ChildStdout) -> (Connection, IoThreads) {
        let responses = Arc::new(Mutex::new(VecDeque::new()));
        let notifications = Arc::new(Mutex::new(VecDeque::new()));
        let requests = Arc::new(Mutex::new(VecDeque::new()));
        let exit_reader = Arc::new(Mutex::new(false));
        let exit_writer = Arc::new(Mutex::new(false));

        let responses2 = Arc::clone(&responses);
        let notifications2 = Arc::clone(&notifications);
        let requests2 = Arc::clone(&requests);
        let exit_reader2 = Arc::clone(&exit_reader);
        let exit_writer2 = Arc::clone(&exit_writer);
        let (sender, receiver, io_threads) = stdio::stdio_transport(
            stdin,
            stdout,
            responses2,
            notifications2,
            requests2,
            exit_reader2,
            exit_writer2,
        );
        (
            Connection {
                sender,
                receiver,
                responses,
                notifications,
                requests,
                exit_reader,
                exit_writer,
            },
            io_threads,
        )
    }

    /// Open a connection over tcp.
    /// This call blocks until a connection is established.
    ///
    /// Use this to create a real language server.
    pub fn connect<A: ToSocketAddrs>(addr: A) -> io::Result<(Connection, IoThreads)> {
        let responses = Arc::new(Mutex::new(VecDeque::new()));
        let notifications = Arc::new(Mutex::new(VecDeque::new()));
        let requests = Arc::new(Mutex::new(VecDeque::new()));
        let exit_reader = Arc::new(Mutex::new(false));
        let exit_writer = Arc::new(Mutex::new(false));

        let responses2 = Arc::clone(&responses);
        let notifications2 = Arc::clone(&notifications);
        let requests2 = Arc::clone(&requests);
        let exit_reader2 = Arc::clone(&exit_reader);
        let exit_writer2 = Arc::clone(&exit_reader);

        let stream = TcpStream::connect(addr)?;
        let (sender, receiver, io_threads) = socket::socket_transport(
            stream,
            responses2,
            notifications2,
            requests2,
            exit_reader2,
            exit_writer2,
        );
        Ok((
            Connection {
                sender,
                receiver,
                responses,
                notifications,
                requests,
                exit_reader,
                exit_writer,
            },
            io_threads,
        ))
    }
}
