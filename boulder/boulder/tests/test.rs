#[macro_use]
extern crate serial_test_derive;
extern crate boulder;

use std::{
    fmt,
    fs::File,
    io::{Error, ErrorKind, Read, Write},
    panic,
    sync::{Arc, Mutex},
};

use walkdir::WalkDir;

use regex::Regex;

use diagnostics::CompileError;

use shared_id::{TypeId, BOOL_TYPE_ID, FALSE_TYPE_ID, TRUE_TYPE_ID};

use mir::{Mir, Object, Type};

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
#[serial]
fn compile_fail() {
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

            let content = match panic::catch_unwind(|| boulder::compile(&content)) {
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
                        "[boulder/{}]: did not contain `{}`:\n{}",
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
        panic!("{} out of {} tests failed", count - success, count);
    } else if count == 0 {
        panic!("No `compile_run` tests found");
    }
}

fn str_to_obj(mir: &Mir, s: &str, ty: TypeId) -> Object {
    let s = s.trim();
    match &mir[ty] {
        &Type::U8 => Object::U8(s.parse().expect("parse u8")),
        &Type::U16 => Object::U16(s.parse().expect("parse u16")),
        &Type::U32 => Object::U32(s.parse().expect("parse u32")),
        _ => {
            if ty == TRUE_TYPE_ID {
                assert_eq!(s, "True");
                Object::Unit
            } else if ty == FALSE_TYPE_ID {
                assert_eq!(s, "False");
                Object::Unit
            } else if ty == BOOL_TYPE_ID {
                if s == "True" {
                    Object::Variant(TRUE_TYPE_ID, Box::new(Object::Unit))
                } else if s == "False" {
                    Object::Variant(FALSE_TYPE_ID, Box::new(Object::Unit))
                } else {
                    panic!("invalid Bool")
                }
            } else {
                panic!("unknown type")
            }
        }
    }
}

fn test_mir(
    mir: &Mir,
    test_regex: &Regex,
    unit_tests: &[&str],
    entry_path: impl fmt::Display,
) -> bool {
    let mut bmi = mir_interpreter::BoulderMirInterpreter::new(mir);
    let mut check_count = 0;
    for test in unit_tests {
        match panic::catch_unwind(panic::AssertUnwindSafe(|| {
            check_count += 1;
            let captures = test_regex.captures(test).expect("invalid test string");
            let fn_name = captures.get(1).unwrap();
            let args = captures.get(2).unwrap();
            let result = captures.get(3).unwrap();
            let func = mir
                .get_function(fn_name.as_str().trim())
                .expect("unknown function name");
            let args: Vec<_> = args
                .as_str()
                .split(",")
                .zip(mir[func].args())
                .map(|(arg, &ty)| str_to_obj(&mir, arg, ty))
                .collect();

            let result = str_to_obj(&mir, result.as_str(), mir[func].ret);
            assert_eq!(
                bmi.execute_function(func, &args)
                    .expect("function execution error"),
                result
            );
        })) {
            Ok(()) => (),
            Err(_) => {
                eprintln!(
                    "[boulder/{}]: panic during unit test `{}`",
                    entry_path, test
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

#[test]
#[serial]
fn compile_run() {
    let mut count = 0;
    let mut success = 0;
    let test_regex =
        Regex::new(r#"fn ([[[:alnum:]]_]*)\(([[[:alpha:]]0-9, ]*)\) -> ([[[:alpha:]]0-9]*)"#)
            .unwrap();
    for entry in WalkDir::new("tests/compile_run") {
        let entry = entry.unwrap();
        if entry.metadata().unwrap().is_file() {
            count += 1;
            let mut file = File::open(entry.path()).unwrap();
            let mut content = String::new();
            file.read_to_string(&mut content).unwrap();

            // TODO: check the interpretation result
            let unit_tests = content
                .lines()
                .take_while(|l| l.starts_with("# "))
                .map(|l| &l["# ".len()..])
                .collect::<Vec<_>>();

            let output = Arc::new(Mutex::new(String::new()));
            CompileError::set_output(Box::new(OutputShim {
                inner: output.clone(),
            }));

            let mir = match panic::catch_unwind(|| boulder::compile_to_mir(&content)) {
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
                if !test_mir(&mir, &test_regex, &unit_tests, entry.path().display()) {
                    continue;
                }

                boulder::core_optimizations(&mut mir);
                if !test_mir(&mir, &test_regex, &unit_tests, entry.path().display()) {
                    continue;
                }

                mir.reduce_binops();
                if !test_mir(&mir, &test_regex, &unit_tests, entry.path().display()) {
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
        panic!("{} out of {} tests failed", count - success, count);
    } else if count == 0 {
        panic!("No `compile_run` tests found");
    }
}
