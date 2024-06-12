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
    exit: Arc<Mutex<bool>>,
) -> (Sender<Message>, Receiver<Message>, IoThreads) {
    let exit_reader = Arc::clone(&exit);
    let (reader_receiver, reader) = make_reader(stream.try_clone().unwrap(), exit_reader);

    let exit_writer = Arc::clone(&exit);
    let (writer_sender, writer) = make_writer(stream.try_clone().unwrap(), exit_writer);

    let io_threads = make_io_threads(reader, writer);
    (writer_sender, reader_receiver, io_threads)
}

fn make_reader(
    stream: TcpStream,
    exit: Arc<Mutex<bool>>,
) -> (Receiver<Message>, thread::JoinHandle<io::Result<()>>) {
    let (sender_to_client, receiver_for_client) = bounded::<Message>(0);
    let reader_thread = thread::spawn(move || {
        let mut buf_read = BufReader::new(stream);
        while let Some(msg) = Message::read(&mut buf_read).unwrap() {
            Logger::debug(&format!("socket read {:#?}", &msg));

            match sender_to_client.send(msg) {
                Ok(_) => {}
                Err(e) => {
                    Logger::error(&format!("send to client error {}", e));
                }
            }

            let exit = exit.lock().unwrap();
            if *exit {
                Logger::info("socket write finished.");
                return Ok(());
            }
        }
        Ok(())
    });
    (receiver_for_client, reader_thread)
}

fn make_writer(
    mut stream: TcpStream,
    exit_writer: Arc<Mutex<bool>>,
) -> (Sender<Message>, thread::JoinHandle<io::Result<()>>) {
    let (sender_for_client, receiver_from_client) = bounded::<Message>(0);
    let writer_thread = thread::spawn(move || {
        receiver_from_client
            .into_iter()
            .try_for_each(|it| {
                let exit = exit_writer.lock().unwrap();
                if *exit {
                    Logger::info("stdio write finished.");
                    return Ok(());
                }

                Logger::debug(&format!("stdio write {:#?}", &it));

                it.write(&mut stream)
            })
            .unwrap();
        Ok(())
    });
    (sender_for_client, writer_thread)
}
