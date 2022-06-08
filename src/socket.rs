use std::{
    collections::VecDeque,
    io::{self, BufReader},
    net::TcpStream,
    sync::{Arc, Mutex},
    thread,
};

use crossbeam_channel::{bounded, Receiver, Sender};

use crate::{
    connection::{NOTIFICATION_MAX, REQUEST_MAX},
    logger::Logger,
    msg::Message,
    stdio::{make_io_threads, IoThreads},
};

pub(crate) fn socket_transport(
    stream: TcpStream,
    message_queue: Arc<Mutex<VecDeque<Message>>>,
    exit: Arc<Mutex<bool>>,
) -> (Sender<Message>, Receiver<Message>, IoThreads) {
    let exit_reader = Arc::clone(&exit);
    let (reader_receiver, reader) =
        make_reader(stream.try_clone().unwrap(), message_queue, exit_reader);

    let exit_writer = Arc::clone(&exit);
    let (writer_sender, writer) = make_writer(stream.try_clone().unwrap(), exit_writer);

    let io_threads = make_io_threads(reader, writer);
    (writer_sender, reader_receiver, io_threads)
}

fn make_reader(
    stream: TcpStream,
    message_queue: Arc<Mutex<VecDeque<Message>>>,
    exit: Arc<Mutex<bool>>,
) -> (Receiver<Message>, thread::JoinHandle<io::Result<()>>) {
    let (reader_sender, reader_receiver) = bounded::<Message>(0);
    let reader = thread::spawn(move || {
        let mut buf_read = BufReader::new(stream);
        while let Some(msg) = Message::read(&mut buf_read).unwrap() {
            let mut queue = message_queue.lock().unwrap();

            Logger::log(&format!("stdio read {:#?}", &msg));
            queue.push_back(msg);

            let exit = exit.lock().unwrap();
            if *exit {
                Logger::log("socket write finished.");
                return Ok(());
            }
        }
        Ok(())
    });
    (reader_receiver, reader)
}

fn make_writer(
    mut stream: TcpStream,
    exit_writer: Arc<Mutex<bool>>,
) -> (Sender<Message>, thread::JoinHandle<io::Result<()>>) {
    let (writer_sender, writer_receiver) = bounded::<Message>(0);
    let writer = thread::spawn(move || {
        writer_receiver
            .into_iter()
            .try_for_each(|it| {
                let exit = exit_writer.lock().unwrap();
                if *exit {
                    Logger::log("stdio write finished.");
                    return Ok(());
                }

                Logger::log(&format!("stdio write {:#?}", &it));

                it.write(&mut stream)
            })
            .unwrap();
        Ok(())
    });
    (writer_sender, writer)
}
