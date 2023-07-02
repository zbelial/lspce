use std::time::Instant;
use std::{
    collections::VecDeque,
    io::{self, stdin, stdout},
    ops::ControlFlow,
    process::{ChildStdin, ChildStdout},
    sync::{Arc, Mutex},
    thread,
};

use bytes::BytesMut;
use crossbeam_channel::{bounded, Receiver, Sender};

use crate::logger::log_enabled;
use crate::msg::{Message, Response, RequestId, ErrorCode};
use crate::{
    connection::{NOTIFICATION_MAX, REQUEST_MAX},
    logger::Logger,
};

/// Creates an LSP connection via stdio.
pub(crate) fn stdio_transport(
    mut child_stdin: ChildStdin,
    mut child_stdout: ChildStdout,
    exit: Arc<Mutex<bool>>,
) -> (Sender<Message>, Receiver<Message>, IoThreads) {
    let exit_writer = Arc::clone(&exit);
    let (sender_for_client, receiver_from_client) = bounded::<Message>(10);
    let writer_thread = thread::spawn(move || {
        let mut stdin = child_stdin;
        loop {
            {
                match exit_writer.lock() {
                    Ok(exit) => {
                        if *exit {
                            Logger::log(&format!("stdio write exited"));
                            break;
                        }
                    }
                    Err(e) => {
                        Logger::log(&format!("stdio writer exit error {}", e));
                    }
                }
            }
            let recv_value = receiver_from_client.recv_timeout(std::time::Duration::from_millis(10));
            match recv_value {
                Ok(r) => {
                    Logger::log(&format!(
                        "stdio write {}",
                        serde_json::to_string_pretty(&r).unwrap_or("invalid json".to_string())
                    ));

                    r.write(&mut stdin)
                }
                Err(t) => Ok(()),
            };
        }
        Logger::log(&format!("stdio write finished"));
        Ok(())
    });

    let exit_reader = Arc::clone(&exit);
    let (sender_to_client, receiver_for_client) = bounded::<Message>(10);
    let reader_thread = thread::spawn(move || {
        let mut stdout = child_stdout;
        let mut reader = std::io::BufReader::new(stdout);

        loop {
            {
                match exit_reader.lock() {
                    Ok(exit) => {
                        if *exit {
                            Logger::log(&format!("stdio read exited"));
                            break;
                        }
                    }
                    Err(e) => {
                        Logger::log(&format!("stdio read exit error {}", e));
                    }
                }
            }
            match Message::read(&mut reader) {
                Ok(m) => {
                    if let Some(msg) = m {
                        if log_enabled() {
                            let msg_log = msg.clone();
                            match msg_log {
                                Message::Request(r) => {
                                    Logger::log(&format!("stdio read request {}", &r.content))
                                }
                                Message::Response(r) => {
                                    Logger::log(&format!("stdio read response {}", &r.content))
                                }
                                Message::Notification(r) => {
                                    Logger::log(&format!("stdio read notification {}", &r.content))
                                }
                            }
                        }
                        let r = sender_to_client.send(msg);
                        if r.is_err() {
                            Logger::log(&format!("stdio read error {}", r.err().unwrap()));
                        }
                    } else {
                        // Logger::log(&format!("stdio read null"));
                        thread::sleep(std::time::Duration::from_millis(10));
                    }
                }
                Err(e) => {
                    Logger::log(&format!("stdio read error {}", e));

                    let msg = Response::new_err(RequestId::from(1), -32603, format!("{}", e));
                    sender_to_client.send(Message::Response(msg));
                }
            }
        }

        Logger::log(&format!("stdio read finished"));
        Ok(())
    });
    let threads = IoThreads { reader: reader_thread, writer: writer_thread };
    (sender_for_client, receiver_for_client, threads)
}

// Creates an IoThreads
pub(crate) fn make_io_threads(
    reader: thread::JoinHandle<io::Result<()>>,
    writer: thread::JoinHandle<io::Result<()>>,
) -> IoThreads {
    IoThreads { reader, writer }
}

pub struct IoThreads {
    reader: thread::JoinHandle<io::Result<()>>,
    writer: thread::JoinHandle<io::Result<()>>,
}

impl IoThreads {
    pub fn join(self) -> io::Result<()> {
        Logger::log("IoThreads join");

        match self.reader.join() {
            Ok(r) => r?,
            Err(err) => {
                println!("reader panicked!");
                std::panic::panic_any(err)
            }
        };
        match self.writer.join() {
            Ok(r) => r,
            Err(err) => {
                println!("writer panicked!");
                std::panic::panic_any(err);
            }
        };
        Logger::log("IoThreads join finished.");

        Ok(())
    }
}
