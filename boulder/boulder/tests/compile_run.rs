extern crate boulder;

use std::{
    fmt,
    fs::File,
    io::{Error, ErrorKind, Read, Write},
    panic,
    sync::{Arc, Mutex},
};

use walkdir::WalkDir;

use diagnostics::CompileError;

use shared_id::{FunctionId, BOOL_TYPE_ID, TRUE_TYPE_ID};

use mir::{Mir, Object};

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

fn test_mir(mir: &Mir, entry_path: impl fmt::Display) -> bool {
    let mut bmi = mir_interpreter::BoulderMirInterpreter::new(mir);
    let mut check_count = 0;
    for (id, test) in mir
        .functions
        .iter()
        .enumerate()
        .filter(|(_, func)| func.ctx.is_test)
    {
        check_count += 1;
        if !test.args().is_empty() || test.ret != BOOL_TYPE_ID {
            eprintln!(
                "[boulder/{}]: invalid test function, expected `() -> Bool`: {}",
                entry_path, test.name
            );
            return false;
        }

        match panic::catch_unwind(panic::AssertUnwindSafe(|| {
            bmi.execute_function(FunctionId::from(id), &[])
        })) {
            Ok(Ok(obj)) => {
                if let Object::Variant(TRUE_TYPE_ID, _) = &obj {
                } else {
                    eprintln!(
                        "[boulder/{}]: unit test `{}` failed: {:?}",
                        entry_path, test.name, obj
                    );
                    return false;
                }
            }
            Ok(Err(err)) => {
                eprintln!(
                    "[boulder/{}]: interpreter during unit test `{}`: {:?}",
                    entry_path, test.name, err
                );
                return false;
            }
            Err(_) => {
                eprintln!(
                    "[boulder/{}]: panic during unit test `{}`",
                    entry_path, test.name
                );
                return false;
            }
        }
    }

    if check_count == 0 {
        eprintln!("[boulder/{}]: did not check test any function", entry_path,);
        false
    } else {
        true
    }
}

#[derive(Debug)]
struct TestFailure;

#[test]
fn compile_run() -> Result<(), TestFailure> {
    let mut count = 0;
    let mut success = 0;
    for entry in WalkDir::new("tests/compile_run") {
        let entry = entry.unwrap();
        if entry.metadata().unwrap().is_file() {
            count += 1;
            let mut file = File::open(entry.path()).unwrap();
            let mut content = String::new();
            file.read_to_string(&mut content).unwrap();

            let expected: Vec<_> = content
                .lines()
                .take_while(|l| l.starts_with("# "))
                .map(|l| l["# ".len()..].trim())
                .collect();

            let should_run = !expected.contains(&"no_run");

            let output = Arc::new(Mutex::new(String::new()));
            CompileError::set_output(Box::new(OutputShim {
                inner: output.clone(),
            }));

            let mir = match panic::catch_unwind(|| {
                boulder::compile_to_mir(&content, &entry.path().to_string_lossy())
            }) {
                Ok(c) => c,
                Err(_) => {
                    let output = output.lock().unwrap();
                    eprintln!(
                        "[boulder/{}]: panic during compilation, output:\n{}",
                        entry.path().display(),
                        output
                    );
                    continue;
                }
            };
            let output = output.lock().unwrap();
            if let Ok(mut mir) = mir {
                if should_run && !test_mir(&mir, entry.path().display()) {
                    continue;
                }

                boulder::core_optimizations(&mut mir);
                if should_run && !test_mir(&mir, entry.path().display()) {
                    continue;
                }

                mir.reduce_binops();
                if should_run && !test_mir(&mir, entry.path().display()) {
                    continue;
                }
                success += 1;
            } else {
                eprintln!(
                    "[boulder/{}]: failed to compile:\n{}\n",
                    entry.path().display(),
                    output
                );
            }
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