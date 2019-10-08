extern crate boulder;

use std::{
    fs::File,
    io::{Error, ErrorKind, Read, Write},
    panic,
    sync::{Arc, Mutex},
};

use walkdir::WalkDir;

use diagnostics::CompileError;

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

#[derive(Debug)]
struct TestFailure;

#[test]
fn compile_fail() -> Result<(), TestFailure> {
    let mut count = 0;
    let mut success = 0;

    'outer: for entry in WalkDir::new("tests/compile_fail") {
        let entry = entry.unwrap();
        if entry.metadata().unwrap().is_file() {
            count += 1;
            let mut file = File::open(entry.path()).unwrap();
            let mut content = String::new();
            file.read_to_string(&mut content).unwrap();

            let expected = content
                .lines()
                .take_while(|l| l.starts_with("# "))
                .map(|l| &l["# ".len()..]);

            let output = Arc::new(Mutex::new(String::new()));
            CompileError::set_output(Box::new(OutputShim {
                inner: output.clone(),
            }));

            let content = match panic::catch_unwind(|| {
                boulder::compile(&content, &entry.path().to_string_lossy())
            }) {
                Ok(c) => c,
                Err(_) => {
                    let output = output.lock().unwrap();
                    eprintln!(
                        "[boulder/{}]: panic during compilation, output:\n{}",
                        entry.path().display(),
                        output
                    );
                    continue 'outer;
                }
            };

            if content.is_ok() {
                eprintln!(
                    "[boulder/{}]: did not fail to compile",
                    entry.path().display()
                );
                continue 'outer;
            }

            let output = output.lock().unwrap();
            let mut check_count = 0;
            for expected in expected {
                check_count += 1;
                if !output.contains(expected) {
                    eprintln!(
                        "[boulder/{}]: did not contain \"{}\":\n{}",
                        entry.path().display(),
                        expected,
                        output.trim(),
                    );
                    continue 'outer;
                }
            }

            if check_count == 0 {
                eprintln!(
                    "[boulder/{}]: did not check any error messages, actual output:\n{}",
                    entry.path().display(),
                    output.trim(),
                );
                continue 'outer;
            }

            success += 1;
        }
    }

    if success != count {
        eprintln!("{} out of {} tests failed", count - success, count);
        Err(TestFailure)
    } else if count == 0 {
        eprintln!("No `compile_run` tests found");
        Err(TestFailure)
    } else {
        Ok(())
    }
}
