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

use shared_id::{FunctionId, TRUE_TYPE_ID};

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

fn test_mir(mir: &Mir, stage: &str) {
    #[cfg(feature = "profiler")]
    profile_scope!("test_mir");
    let mut bmi = mir_interpreter::BoulderMirInterpreter::new(mir);
    let mut check_count = 0;
    for (id, test) in mir
        .functions
        .iter()
        .enumerate()
        .filter(|(_, func)| func.ctx.is_test)
    {
        check_count += 1;
        match panic::catch_unwind(panic::AssertUnwindSafe(|| {
            bmi.execute_function(FunctionId::from(id), &[])
        })) {
            Ok(Ok(obj)) => {
                if let Object::Variant(TRUE_TYPE_ID, _) = &obj {
                } else {
                    panic!(
                        "unit test `{}` failed at stage `{}`: {:?}",
                        test.name, stage, obj
                    )
                }
            }
            Ok(Err(err)) => panic!(
                "interpreter during unit test `{}` at stage `{}`: {:?}",
                test.name, stage, err
            ),
            Err(_) => panic!(
                "panic during unit test `{}`  at stage `{}`",
                test.name, stage
            ),
        }
    }

    if check_count == 0 {
        panic!("did not check test any function at stage `{}`", stage)
    }
}

#[derive(Debug)]
struct TestFailure;

#[test]
fn compile_run() -> Result<(), TestFailure> {
    #[cfg(feature = "thread_profiler")]
    thread_profiler::register_thread_with_profiler();
    let mut count = 0;
    let mut success = 0;
    for entry in WalkDir::new("tests/compile_run") {
        let entry = entry.unwrap();
        if entry.metadata().unwrap().is_file() {
            #[cfg(feature = "profiler")]
            profile_scope!(format!("{}", entry.path().display()));
            let ctx = GlobalCtx::new();
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

            let s = entry.path().to_string_lossy();
            match panic::catch_unwind(AssertUnwindSafe(|| {
                let mir = boulder::compile_to_mir(&ctx, &content, &s);
                if let Ok(mut mir) = mir {
                    #[cfg(feature = "profiler")]
                    profile_scope!("optimize_and_test");
                    if should_run {
                        test_mir(&mir, "initial");
                    }

                    boulder::core_optimizations(&mut mir);
                    if should_run {
                        test_mir(&mir, "core optimizations");
                    }

                    mir.reduce_binops();
                    mir.validate();
                    if should_run {
                        test_mir(&mir, "reduced binops");
                    }

                    mir.reduce_sum_types();
                    mir.validate();
                    if should_run {
                        test_mir(&mir, "reduced sum types");
                    }
                } else {
                    let output = output.lock().unwrap();
                    panic!("failed to compile:\n{}\n", output)
                }
            })) {
                Ok(()) => (),
                Err(_) => {
                    let output = output.lock().unwrap();
                    eprintln!(
                        "[boulder/{}]: panic during testing compilation, compiler output:\n{}",
                        entry.path().display(),
                        output
                    );
                    continue;
                }
            };

            success += 1;
        }
    }

    #[cfg(feature = "thread_profiler")]
    {
        let output_file = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), "run.profile.json");
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
