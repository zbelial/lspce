use std::{
    collections::VecDeque,
    io::{self, stdin, stdout},
    process::{ChildStdin, ChildStdout},
    sync::{Arc, Mutex},
    thread,
};

use bytes::BytesMut;
use crossbeam_channel::{bounded, Receiver, Sender};

use crate::{logger::Logger, msg::Message};

/// Creates an LSP connection via stdio.
pub(crate) fn stdio_transport(
    mut stdin: ChildStdin,
    mut stdout: ChildStdout,
    msgs: Arc<Mutex<VecDeque<Message>>>,
) -> (Sender<Message>, Receiver<Message>, IoThreads) {
    let (writer_sender, writer_receiver) = bounded::<Message>(0);
    let writer = thread::spawn(move || {
        writer_receiver.into_iter().try_for_each(|it| {
            Logger::log(&format!("stdio write {:#?}", &it));

            it.write(&mut stdin)
        })?;
        Ok(())
    });

    let (reader_sender, reader_receiver) = bounded::<Message>(0);
    let reader = thread::spawn(move || {
        let mut bytes_mut = BytesMut::with_capacity(65536);
        while let Some(msg) = Message::read(&mut stdout, &mut bytes_mut)? {
            Logger::log(&format!("stdio read {:#?}", &msg));

            msgs.lock().unwrap().push_back(msg);
        }
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
        match self.reader.join() {
            Ok(r) => r?,
            Err(err) => {
                println!("reader panicked!");
                std::panic::panic_any(err)
            }
        }
        match self.writer.join() {
            Ok(r) => r,
            Err(err) => {
                println!("writer panicked!");
                std::panic::panic_any(err);
            }
        }
    }
}
