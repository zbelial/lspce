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
    process::{ChildStderr, ChildStdin, ChildStdout},
    sync::{Arc, Mutex},
};

use crossbeam_channel::{Receiver, SendError, Sender};

use crate::{
    logger::Logger,
    msg::{Message, Notification, Request},
    stdio::IoThreads,
};
use crate::{socket, stdio};

pub const NOTIFICATION_MAX: usize = 10;
pub const REQUEST_MAX: usize = 10;

/// Connection is just a pair of channels of LSP messages.
pub struct Connection {
    sender: Sender<Message>,
    receiver: Receiver<Message>,
    exit: Arc<Mutex<bool>>,
}

impl Connection {
    pub fn write(&self, req: Message) -> Result<(), SendError<Message>> {
        self.sender.send(req)
    }

    pub fn read(&self) -> Option<Message> {
        match self
            .receiver
            .recv_timeout(std::time::Duration::from_millis(1))
        {
            Ok(q) => Some(q),
            Err(e) => {
                // Logger::error(&format!("Connection read error {}", e));
                None
            }
        }
    }

    pub fn to_exit(&self) {
        match self.exit.lock() {
            Ok(mut e) => {
                *e = true;
            }
            Err(e) => {
                Logger::error(&format!("Connection to_exit error {}", e));
            }
        }
        Logger::info("after exit in Connection.");
    }

    /// Create connection over standard in/standard out.
    ///
    /// Use this to create a real language server.
    pub fn stdio(mut stdin: ChildStdin, mut stdout: ChildStdout, mut stderr: ChildStderr) -> (Connection, IoThreads) {
        let exit = Arc::new(Mutex::new(false));

        let exit2 = Arc::clone(&exit);
        let (sender, receiver, io_threads) = stdio::stdio_transport(stdin, stdout, stderr, exit2);
        (
            Connection {
                sender,
                receiver,
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
        let exit = Arc::new(Mutex::new(false));

        let exit2 = Arc::clone(&exit);

        let stream = TcpStream::connect(addr)?;
        let (sender, receiver, io_threads) = socket::socket_transport(stream, exit2);
        Ok((
            Connection {
                sender,
                receiver,
                exit,
            },
            io_threads,
        ))
    }
}
