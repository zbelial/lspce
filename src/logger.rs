use std::{
    fmt,
    fs::File,
    io::{self, BufRead, Error, Read, Write},
    mem::MaybeUninit,
    path::Path,
    sync::{
        atomic::{AtomicU8, Ordering},
        Arc, Mutex, Once,
    },
};

use chrono::Local;
use lazy_static::lazy_static;

pub static LOG_ENABLE: AtomicU8 = AtomicU8::new(1);

lazy_static! {
    pub static ref LOG_FILE_NAME: Mutex<String> = Mutex::new(String::new());
}

fn log_enabled() -> bool {
    LOG_ENABLE.load(Ordering::Relaxed) == 1
}

fn log_file_name() -> String {
    LOG_FILE_NAME.lock().unwrap().clone()
}

pub struct Logger {}

impl Logger {
    pub fn log(buf: &str) {
        if log_enabled() {
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
}

struct FakeFile {}

impl Write for FakeFile {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        Ok(buf.len())
    }
    fn flush(&mut self) -> Result<(), Error> {
        Ok(())
    }
}

fn logger() -> &'static Arc<Mutex<dyn Write>> {
    static mut LOGGER: MaybeUninit<Arc<Mutex<dyn Write>>> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();

    ONCE.call_once(|| unsafe {
        let file_name = log_file_name();
        if file_name.len() > 0 {
            if let Ok(f) = File::options().create(true).append(true).open(file_name) {
                LOGGER.as_mut_ptr().write(Arc::new(Mutex::new(f)))
            } else {
                LOGGER.as_mut_ptr().write(Arc::new(Mutex::new(FakeFile {})))
            }
        } else {
            LOGGER.as_mut_ptr().write(Arc::new(Mutex::new(FakeFile {})))
        }
    });

    unsafe { &*LOGGER.as_mut_ptr() }
}
