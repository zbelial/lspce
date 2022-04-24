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
    sync::{
        mpsc::{Receiver, SendError, Sender},
        Arc, Mutex,
    },
};

// use crossbeam_channel::{Receiver, SendError, Sender};

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
    msgs: Arc<Mutex<VecDeque<Message>>>,
}

impl Connection {
    pub fn write(&self, req: Message) -> Result<(), SendError<Message>> {
        Logger::log(&format!("Connection write {:#?}", &req));

        self.sender.send(req)
    }

    pub fn read(&self) -> Option<Message> {
        let msg = self.msgs.lock().unwrap().pop_front();
        Logger::log(&format!("Connection read {:#?}", &msg));

        msg
    }

    /// Create connection over standard in/standard out.
    ///
    /// Use this to create a real language server.
    pub fn stdio(mut stdin: ChildStdin, mut stdout: ChildStdout) -> (Connection, IoThreads) {
        let msgs = Arc::new(Mutex::new(VecDeque::new()));
        let msgs2 = msgs.clone();
        let (sender, receiver, io_threads) = stdio::stdio_transport(stdin, stdout, msgs2);
        (
            Connection {
                sender,
                receiver,
                msgs,
            },
            io_threads,
        )
    }

    /// Open a connection over tcp.
    /// This call blocks until a connection is established.
    ///
    /// Use this to create a real language server.
    pub fn connect<A: ToSocketAddrs>(addr: A) -> io::Result<(Connection, IoThreads)> {
        let msgs = Arc::new(Mutex::new(VecDeque::new()));
        let msgs2 = msgs.clone();
        let stream = TcpStream::connect(addr)?;
        let (sender, receiver, io_threads) = socket::socket_transport(stream, msgs2);
        Ok((
            Connection {
                sender,
                receiver,
                msgs,
            },
            io_threads,
        ))
    }
}
