#[macro_use]
extern crate serial_test_derive;
extern crate rock;

use walkdir::WalkDir;

use std::{
    fs::File,
    io::Read,
    panic::{self, AssertUnwindSafe},
};

struct OutputShim {
    inner: String,
}

impl rock::Logger for OutputShim {
    fn log_err(&mut self, err: rock::Error<'_>) {
        self.inner = format!("{}\n{:?}", self.inner, err);
    }
}

#[test]
#[serial]
fn compile_fail() -> Result<(), std::io::Error> {
    let mut count = 0;

    for entry in WalkDir::new("tests/compile_fail") {
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

            let mut output = OutputShim {
                inner: String::new(),
            };

            let content = match panic::catch_unwind(AssertUnwindSafe(|| {
                rock::compile(&content, &mut output)
            })) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("panic in {}", entry.path().display());
                    panic::resume_unwind(e)
                }
            };

            assert!(
                content.is_err(),
                "`{}` did not fail to compile",
                entry.path().display()
            );
            let output = output.inner;
            let mut count = 0;
            for expected in expected {
                count += 1;
                assert!(
                    output.contains(expected),
                    "{}: `{}` did not contain `{}`",
                    entry.path().display(),
                    output.trim(),
                    expected,
                );
            }
            assert_ne!(
                count,
                0,
                "`{}` did not check any error messages, actual output:\n{}",
                entry.path().display(),
                output.trim(),
            );
        }
    }

    assert_ne!(count, 0);
}

#[test]
#[serial]
fn compile_run() {
    let mut count = 0;

    for entry in WalkDir::new("tests/compile_run") {
        let entry = entry.unwrap();
        if entry.metadata().unwrap().is_file() {
            count += 1;
            let mut file = File::open(entry.path()).unwrap();
            let mut content = String::new();
            file.read_to_string(&mut content).unwrap();

            // TODO: check the interpretation result
            let _expected = content
                .lines()
                .take_while(|l| l.starts_with("# "))
                .map(|l| &l["# ".len()..]);

            let mut output = OutputShim {
                inner: String::new(),
            };

            let result = match panic::catch_unwind(AssertUnwindSafe(|| {
                rock::compile(&content, &mut output)
            })) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("panic in {}", entry.path().display());
                    panic::resume_unwind(e)
                }
            };
            let output = output.inner;
            assert!(
                result.is_ok(),
                "`{}` failed to compile: `{}`",
                entry.path().display(),
                output
            );
            // TODO: interpret the compilation result
        }
    }

    assert_ne!(count, 0);
}
