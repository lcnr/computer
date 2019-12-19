#[cfg(feature = "profiler")]
extern crate thread_profiler;

extern crate boulder;

use global_ctx::GlobalCtx;

use std::env;
use std::fs::File;
use std::io::{Read, Write};

const USAGE: &str = "usage: boulder <input file> [<output file>]";

pub fn main() {
    #[cfg(feature = "thread_profiler")]
    thread_profiler::register_thread_with_profiler();
    let mut args = env::args().skip(1);
    if let Some(ref input) = args.next() {
        let output = args.next();
        let output = output.as_ref().map(|t| &**t).unwrap_or("./a.data");
        if let Some(_) = args.next() {
            eprintln!("{}", USAGE);
            return;
        }

        if let Ok(mut file) = File::open(input) {
            let ctx = GlobalCtx::new();

            let mut src = String::new();
            if let Err(err) = file.read_to_string(&mut src) {
                eprintln!("Error while reading {}: {:?}", input, err);
            } else if let Ok(res) = boulder::compile(&ctx, &src, input) {
                let mut miri = mir_interpreter::BoulderMirInterpreter::new(&res);
                println!("{}", res);
                dbg!(miri.execute_function(res.get_function("simple").unwrap(), &[]));

                if let Ok(mut file) = File::create(output) {
                    writeln!(file, "{}", res).expect("error while writing to file");
                    /*writeln!(file, "v2.0 raw").expect("error while writing to file");
                    for bytes in res.chunks(4) {
                        for b in bytes {
                            write!(file, "{:02x} ", b).expect("error while writing to file");
                        }
                        write!(file, "\n").expect("error while writing to file");
                    }*/
                    println!("Successfully compiled `{}` to `{}`", input, output);
                } else {
                    eprintln!("unable to create file: {}", output);
                }
            } else {
                println!("Unable to compile `{}`", input);
            }
        } else {
            eprintln!("unable to open file: {}", input);
        }
    } else {
        eprintln!("{}", USAGE);
    }

    #[cfg(feature = "thread_profiler")]
    {
        let output_file = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), "main.profile.json");
        println!(
            "Writing profile to {}, try loading this using chome 'about:tracing'",
            output_file
        );
        thread_profiler::write_profile(output_file.as_str());
    }
}
