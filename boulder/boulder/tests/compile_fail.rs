extern crate boulder;

use walkdir::WalkDir;

use boulder_core::CompileError;
use std::{
    fs::File,
    io::{Error, ErrorKind, Read, Write},
    sync::{Arc, Mutex},
};

struct OutputShim {
    inner: Arc<Mutex<String>>,
}

impl Write for OutputShim {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        self.inner.lock().unwrap().push_str(
            std::str::from_utf8(buf)
                .map_err(|_| Error::new(ErrorKind::InvalidData, "received non utf8 data!"))?,
        );
        Ok(buf.len())
    }

    fn flush(&mut self) -> Result<(), Error> {
        Ok(())
    }
}

#[test]
fn compile_fail() -> Result<(), std::io::Error> {
    let mut count = 0;

    for entry in WalkDir::new("tests/compile_fail") {
        let entry = entry.unwrap();
        if entry.metadata().unwrap().is_file() {
            count += 1;
            let mut file = File::open(entry.path())?;
            let mut content = String::new();
            file.read_to_string(&mut content)?;

            let expected = content
                .lines()
                .take_while(|l| l.starts_with("# "))
                .map(|l| &l["# ".len()..]);

            let output = Arc::new(Mutex::new(String::new()));
            CompileError::set_output(Box::new(OutputShim {
                inner: output.clone(),
            }));

            assert!(boulder::compile(&content).is_err(), "`{}` did not fail to compile", entry.path().display());
            let output = output.lock().unwrap();
            for expected in expected {
                assert!(
                    output.contains(expected),
                    "`{}` did not contain `{}` ({})",
                    output.trim(),
                    expected,
                    entry.path().display(),
                );
            }
        }
    }

    assert_ne!(count, 0);
    Ok(())
}
