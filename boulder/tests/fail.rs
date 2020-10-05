#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

extern crate boulder;

use std::{
    fs::File,
    io::{Error, ErrorKind, Read, Write},
    panic::{self, AssertUnwindSafe},
    sync::{Arc, Mutex},
};

use walkdir::WalkDir;

use global_ctx::GlobalCtx;

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

fn main() -> Result<(), TestFailure> {
    #[cfg(feature = "thread_profiler")]
    thread_profiler::register_thread_with_profiler();
    let mut count = 0;
    let mut success = 0;

    'outer: for entry in WalkDir::new("tests/error") {
        let entry = entry.unwrap();
        if entry.metadata().unwrap().is_file() {
            let file_str = format!("boulder/{}", entry.path().display());

            #[cfg(feature = "profiler")]
            profile_scope!(file_str);
            let ctx = GlobalCtx::new();
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

            let content = match panic::catch_unwind(AssertUnwindSafe(|| {
                boulder::compile_to_mir(&ctx, &content, &file_str)
            })) {
                Ok(c) => c,
                Err(_) => {
                    let output = output.lock().unwrap();
                    eprintln!(
                        "[{}]: panic during compilation, output:\n{}",
                        file_str, output
                    );
                    continue 'outer;
                }
            };

            if content.is_ok() {
                eprintln!("[{}]: did not fail to compile", file_str,);
                continue 'outer;
            }

            let output = output.lock().unwrap();
            let mut check_count = 0;
            #[cfg(feature = "profiler")]
            profile_scope!("check_expected");
            for expected in expected {
                check_count += 1;
                if !output.contains(expected) {
                    eprintln!(
                        "[{}]: did not contain \"{}\":\n{}",
                        file_str,
                        expected,
                        output.trim(),
                    );
                    continue 'outer;
                }
            }

            if check_count == 0 {
                eprintln!(
                    "[{}]: did not check any error messages, actual output:\n{}",
                    file_str,
                    output.trim(),
                );
                continue 'outer;
            }

            success += 1;
        }
    }

    #[cfg(feature = "thread_profiler")]
    {
        let output_file = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), "fail.profile.json");
        println!(
            "Writing profile to {}, try loading this using chome 'about:tracing'",
            output_file
        );
        thread_profiler::write_profile(output_file.as_str());
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
