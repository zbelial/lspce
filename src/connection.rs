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

pub const NOTIFICATION_MAX: usize = 10;
pub const REQUEST_MAX: usize = 10;

/// Connection is just a pair of channels of LSP messages.
pub struct Connection {
    sender: Sender<Message>,
    receiver: Receiver<Message>,
    messages: Arc<Mutex<VecDeque<Message>>>,
    exit: Arc<Mutex<bool>>,
}

impl Connection {
    pub fn write(&self, req: Message) -> Result<(), SendError<Message>> {
        self.sender.send(req)
    }

    pub fn read(&self) -> Option<Message> {
        let msg = match self.messages.lock() {
            Ok(mut q) => q.pop_front(),
            Err(e) => {
                Logger::log(&format!("Connect read error {}", e));
                None
            }
        };

        msg
    }

    pub fn to_exit(&self) {
        match self.exit.lock() {
            Ok(mut e) => {
                *e = true;
            }
            Err(e) => {
                Logger::log(&format!("Connection to_exit error {}", e));
            }
        }
        Logger::log("after exit");
    }

    /// Create connection over standard in/standard out.
    ///
    /// Use this to create a real language server.
    pub fn stdio(mut stdin: ChildStdin, mut stdout: ChildStdout) -> (Connection, IoThreads) {
        let messages = Arc::new(Mutex::new(VecDeque::new()));
        let exit = Arc::new(Mutex::new(false));

        let messages2 = Arc::clone(&messages);
        let exit2 = Arc::clone(&exit);
        let (sender, receiver, io_threads) =
            stdio::stdio_transport(stdin, stdout, messages2, exit2);
        (
            Connection {
                sender,
                receiver,
                messages,
                exit,
            },
            io_threads,
        )
    }

    /// Open a connection over tcp.
    /// This call blocks until a connection is established.
    ///
    /// Use this to create a real language server.
    pub fn connect<A: ToSocketAddrs>(addr: A) -> io::Result<(Connection, IoThreads)> {
        let messages = Arc::new(Mutex::new(VecDeque::new()));
        let exit = Arc::new(Mutex::new(false));

        let messages2 = Arc::clone(&messages);
        let exit2 = Arc::clone(&exit);

        let stream = TcpStream::connect(addr)?;
        let (sender, receiver, io_threads) = socket::socket_transport(stream, messages2, exit2);
        Ok((
            Connection {
                sender,
                receiver,
                messages,
                exit,
            },
            io_threads,
        ))
    }
}
