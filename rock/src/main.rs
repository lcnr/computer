extern crate rock;

use std::env;
use std::fs::File;
use std::io::{Read, Write};

const USAGE: &str = "usage: rock <input file> [<output file>]";

pub fn main() {
    let mut args = env::args().skip(1);
    if let Some(ref input) = args.next() {
        let output = args.next();
        let output = output.as_ref().map(|t| &**t).unwrap_or("a.data");
        if let Some(_) = args.next() {
            println!("{}", USAGE);
            return;
        }

        if let Ok(mut file) = File::open(input) {
            let mut src = String::new();
            if let Err(err) = file.read_to_string(&mut src) {
                println!("Error while reading {}: {:?}", input, err);
            }

            if let Ok(data) = rock::compile(&src, &mut rock::DebugLogger) {
                if let Ok(mut file) = File::create(output) {
                    writeln!(file, "v2.0 raw").expect("error while writing to file");
                    for bytes in data.chunks(4) {
                        for b in bytes {
                            write!(file, "{:02x} ", b).expect("error while writing to file");
                        }
                        write!(file, "\n").expect("error while writing to file");
                    }
                } else {
                    println!("unable to create file: {}", output);
                }
            }
        } else {
            println!("unable to open file: {}", input);
        }
    } else {
        println!("{}", USAGE);
    }
}
