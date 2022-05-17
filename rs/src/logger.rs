use std::{
    fmt,
    fs::File,
    io::{self, BufRead, Error, Read, Write},
    mem::MaybeUninit,
    path::Path,
    sync::{Arc, Mutex, Once},
};

use chrono::Local;

pub struct Logger {
    initilized: bool,
}

impl Logger {
    pub fn log(buf: &str) {
        let mut logger = logger().lock().unwrap();
        logger.write_all(
            Local::now()
                .format("%Y-%m-%d %H:%M:%S%.3f - ")
                .to_string()
                .as_bytes(),
        );
        logger.write_all(buf.as_bytes());
        logger.write("\n".as_bytes());
    }
}

struct FakeFile {}

impl Write for FakeFile {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        Ok(0)
    }
    fn flush(&mut self) -> Result<(), Error> {
        Ok(())
    }
}

fn logger() -> &'static Arc<Mutex<dyn Write>> {
    static mut LOGGER: MaybeUninit<Arc<Mutex<dyn Write>>> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();

    ONCE.call_once(|| unsafe {
        let file_name = "/tmp/lspce.log";
        if let Ok(f) = File::options().create(true).append(true).open(file_name) {
            LOGGER.as_mut_ptr().write(Arc::new(Mutex::new(f)))
        } else {
            LOGGER.as_mut_ptr().write(Arc::new(Mutex::new(FakeFile {})))
        }
    });

    unsafe { &*LOGGER.as_mut_ptr() }
}
