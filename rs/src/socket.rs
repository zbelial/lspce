use std::{
    collections::VecDeque,
    io::{self, BufReader},
    net::TcpStream,
    sync::{Arc, Mutex},
    thread,
};

use crossbeam_channel::{bounded, Receiver, Sender};

use crate::{
    msg::Message,
    stdio::{make_io_threads, IoThreads},
};

pub(crate) fn socket_transport(
    stream: TcpStream,
    msgs: Arc<Mutex<VecDeque<Message>>>,
) -> (Sender<Message>, Receiver<Message>, IoThreads) {
    let (reader_receiver, reader) = make_reader(stream.try_clone().unwrap(), msgs);
    let (writer_sender, writer) = make_writer(stream.try_clone().unwrap());
    let io_threads = make_io_threads(reader, writer);
    (writer_sender, reader_receiver, io_threads)
}

fn make_reader(
    stream: TcpStream,
    msgs: Arc<Mutex<VecDeque<Message>>>,
) -> (Receiver<Message>, thread::JoinHandle<io::Result<()>>) {
    let (reader_sender, reader_receiver) = bounded::<Message>(0);
    let reader = thread::spawn(move || {
        let mut buf_read = BufReader::new(stream);
        while let Some(msg) = Message::read(&mut buf_read).unwrap() {
            msgs.lock().unwrap().push_back(msg);
        }
        Ok(())
    });
    (reader_receiver, reader)
}

fn make_writer(mut stream: TcpStream) -> (Sender<Message>, thread::JoinHandle<io::Result<()>>) {
    let (writer_sender, writer_receiver) = bounded::<Message>(0);
    let writer = thread::spawn(move || {
        writer_receiver
            .into_iter()
            .try_for_each(|it| it.write(&mut stream))
            .unwrap();
        Ok(())
    });
    (writer_sender, writer)
}
