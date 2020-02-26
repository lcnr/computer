#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

extern crate boulder;

use std::{
    fs::File,
    io::{Error, ErrorKind, Read, Write},
    ops::Deref,
    panic::{self, AssertUnwindSafe},
    sync::{Arc, Mutex},
};

use walkdir::WalkDir;

use global_ctx::GlobalCtx;

use diagnostics::CompileError;

use shared_id::{FunctionId, TRUE_TYPE_ID};

use mir::{LangItemState, Mir, Object};

use lir::Lir;

use lir_interpreter::Memory;

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

fn test_mir(mir: &Mir, e2b: bool, stage: &str) {
    #[cfg(feature = "profiler")]
    profile_scope!("test_mir");
    let mut bmi = mir_interpreter::BoulderMirInterpreter::new(mir, e2b);
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
                if e2b {
                    if let Object::U8(v) = obj {
                        if v != mir.ctx.true_replacement {
                            panic!(
                                "unit test `{}` failed at stage `{}`: {:?}",
                                test.name, stage, obj
                            )
                        }
                    }
                } else if let Object::Variant(TRUE_TYPE_ID, _) = obj {
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

    if check_count == 0 && !mir.functions.iter().any(|f| f.ctx.export) {
        panic!("did not check test any function at stage `{}`", stage)
    }
}

fn test_lir(lir: &Lir, stage: &str) {
    #[cfg(feature = "profiler")]
    profile_scope!("test_mir");
    let mut bli = lir_interpreter::BoulderLirInterpreter::new(lir);
    let mut check_count = 0;
    for (id, test) in lir
        .functions
        .iter()
        .enumerate()
        .filter(|(_, func)| func.ctx.test)
    {
        check_count += 1;
        match bli.execute_function(FunctionId::from(id), &[]) {
            Ok(v) => {
                if v.as_slice() != &[Memory::Byte(lir.ctx.true_replacement)] as &[_] {
                    panic!(
                        "unit test `{}` failed at stage `{}`: {:?}",
                        test.name, stage, v
                    )
                }
            }
            Err(err) => panic!(
                "interpreter during unit test `{}` at stage `{}`: {:?}: {:?}",
                test.name,
                stage,
                bli.last_step(),
                err
            ),
        }
    }

    if check_count == 0 && !lir.functions.iter().any(|f| f.ctx.export) {
        panic!("did not check test any function at stage `{}`", stage)
    }
}

#[derive(Debug)]
struct TestFailure;

fn main() -> Result<(), TestFailure> {
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

            let output = Arc::new(Mutex::new(String::new()));
            CompileError::set_output(Box::new(OutputShim {
                inner: output.clone(),
            }));

            let s = entry.path().to_string_lossy();
            let mut mir = match panic::catch_unwind(AssertUnwindSafe(|| {
                if let Ok(mir) = boulder::compile_to_mir(&ctx, &content, &s) {
                    mir
                } else {
                    let output: String = { output.lock().unwrap().deref().clone() };
                    panic!("failed to compile:\n{}\n", output)
                }
            })) {
                Ok(mir) => mir,
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
            match panic::catch_unwind(AssertUnwindSafe(|| {
                #[cfg(feature = "profiler")]
                profile_scope!("optimize_and_test");
                test_mir(&mir, false, "initial");

                boulder::core_optimizations(&mut mir, false, LangItemState::Unresolved);
                test_mir(&mir, false, "core optimizations");

                mir.reduce_binops();
                mir.validate(false);
                test_mir(&mir, false, "reduced binops");

                boulder::core_optimizations(&mut mir, false, LangItemState::BinopResolved);
                test_mir(&mir, false, "core optimizations post binops");

                mir.reduce_sum_types();
                mir.validate(false);
                test_mir(&mir, false, "reduced sum types");

                boulder::core_optimizations(&mut mir, false, LangItemState::BinopResolved);
                test_mir(&mir, false, "core optimizations post sum types");

                mir.reduce_to_bytes();
                mir.validate(false);
                test_mir(&mir, false, "reduced to bytes");

                boulder::core_optimizations(&mut mir, false, LangItemState::ToBytesResolved);
                test_mir(&mir, false, "core optimizations post bytes");

                mir.enum_to_byte();
                mir.validate(true);
                test_mir(&mir, true, "enum_to_byte");

                let lir = mir2lir::convert(mir);
                test_lir(&lir, "mir2lir");
            })) {
                Ok(()) => (),
                Err(_) => {
                    eprintln!("[boulder/{}]: panic during testing", entry.path().display(),);
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
