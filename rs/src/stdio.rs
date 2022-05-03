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
use lsp_types::notification;

use crate::{
    logger::Logger,
    msg::{Message, Notification, Request, Response},
};

/// Creates an LSP connection via stdio.
pub(crate) fn stdio_transport(
    mut child_stdin: ChildStdin,
    mut child_stdout: ChildStdout,
    responses: Arc<Mutex<VecDeque<Response>>>,
    notifications: Arc<Mutex<VecDeque<Notification>>>,
    requests: Arc<Mutex<VecDeque<Request>>>,
    exit_reader: Arc<Mutex<bool>>,
    exit_writer: Arc<Mutex<bool>>,
) -> (Sender<Message>, Receiver<Message>, IoThreads) {
    let (writer_sender, writer_receiver) = bounded::<Message>(0);
    let writer = thread::spawn(move || {
        let mut stdin = child_stdin;
        loop {
            let exit = exit_writer.lock().unwrap();
            if *exit {
                Logger::log(&format!("stdio write exited"));
                break;
            }
            let recv_value = writer_receiver.try_recv();
            match recv_value {
                Ok(r) => {
                    Logger::log(&format!("stdio write {:#?}", &r));

                    r.write(&mut stdin)
                }
                Err(t) => {
                    thread::sleep(std::time::Duration::from_millis(50));
                    Ok(())
                }
            };
        }
        Logger::log(&format!("stdio write finished"));
        Ok(())
    });

    let (reader_sender, reader_receiver) = bounded::<Message>(0);
    let reader = thread::spawn(move || {
        let mut stdout = child_stdout;
        let mut reader = std::io::BufReader::new(stdout);

        loop {
            let exit = exit_reader.lock().unwrap();
            if *exit {
                Logger::log(&format!("stdio read exited"));
                break;
            }

            if let Some(msg) = Message::read(&mut reader)? {
                Logger::log(&format!("stdio read {:#?}", &msg));

                match msg {
                    Message::Request(r) => {
                        requests.lock().unwrap().push_back(r);
                    }
                    Message::Notification(r) => {
                        notifications.lock().unwrap().push_back(r);
                    }
                    Message::Response(r) => {
                        responses.lock().unwrap().push_back(r);
                    }
                }
            }
        }

        Logger::log(&format!("stdio read finished"));
        Ok(())
    });
    let threads = IoThreads { reader, writer };
    (writer_sender, reader_receiver, threads)
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
