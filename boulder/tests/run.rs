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

use mir::{Mir, Object};

use lir::Lir;

use lir_interpreter::Memory;

use boulder::{LIR_OPTIMIZATIONS, MIR_OPTIMIZATIONS};

use remu::Remu;

const MAX_LIR_STEPS: usize = 5_000_000;
const MAX_ASM_STEPS: usize = 20_000_000;

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
        match bmi.execute_function(FunctionId::from(id), &[]) {
            Ok(obj) => {
                if mir.ctx.e2b {
                    if let Object::U8(v) = obj {
                        if v != mir.ctx.true_replacement {
                            panic!(
                                "unit test `{}` failed at stage `mir::{}`: {:?}",
                                test.name, stage, obj
                            )
                        }
                    }
                } else if !matches!(obj, Object::Variant(TRUE_TYPE_ID, _)) {
                    panic!(
                        "unit test `{}` failed at stage `mir::{}`: {:?}",
                        test.name, stage, obj
                    )
                }
            }
            Err(err) => panic!(
                "interpreter during unit test `{}` at stage `mir::{}`: {:?}\n{}",
                test.name, stage, err, mir
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
        bli.reset_step_count();
        match bli.execute_function(FunctionId::from(id), &[], 100, MAX_LIR_STEPS) {
            Ok(v) => {
                if v.as_slice() != &[Memory::Byte(lir.ctx.true_replacement)] as &[_] {
                    panic!(
                        "unit test `{}` failed at stage `lir::{}`: {:?}",
                        test.name, stage, v
                    )
                }
            }
            Err(err) => panic!(
                "interpreter during unit test `{}` at stage `lir::{}`: {:?}: {:?}",
                test.name,
                stage,
                bli.last_step(),
                err
            ),
        }
    }

    if check_count == 0 && !lir.functions.iter().any(|f| f.ctx.export) {
        panic!("did not check test any function at stage `lir::{}`", stage)
    }
}

#[derive(Debug)]
struct TestFailure;

fn main() -> Result<(), TestFailure> {
    #[cfg(feature = "thread_profiler")]
    thread_profiler::register_thread_with_profiler();
    let mut count = 0;
    let mut success = 0;
    for entry in WalkDir::new("tests/compile") {
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

                test_mir(&mir, "initial");
                for (opt, name) in MIR_OPTIMIZATIONS.iter() {
                    opt(&mut mir);
                    mir.validate();
                    test_mir(&mir, name);
                }

                let mut lir = mir2lir::convert(mir);
                lir.validate();
                test_lir(&lir, "mir2lir");
                for (opt, name) in LIR_OPTIMIZATIONS.iter() {
                    opt(&mut lir);
                    lir.validate();
                    test_lir(&lir, name);
                }

                let asm = lir2asm::convert(&lir);

                let data = rock::compile(&asm, &mut rock::DebugLogger).unwrap();

                let mut remu = Remu::new();
                remu.memory_mut()[0..data.len()].copy_from_slice(&data);

                if lir.functions.iter().any(|f| f.ctx.test) {
                    match remu.run(MAX_ASM_STEPS) {
                        Ok(steps) => println!(
                            "boulder/{:51}{:9} {:5}",
                            entry.path().display(),
                            steps,
                            data.len()
                        ),
                        Err(e) => panic!("{:?}", e),
                    }
                }
            })) {
                Ok(()) => {}
                Err(_) => {
                    eprintln!("[boulder/{}]: panic during testing", entry.path().display());
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
