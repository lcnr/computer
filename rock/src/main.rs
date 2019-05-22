extern crate rock;

use std::env;
use std::fs::File;
use std::io::{Read, Write};

pub fn main() {
    if let [ref input, ref output] = *env::args().skip(1).collect::<Box<_>>() {
        if let Ok(mut file) = File::open(input) {
            let mut src = String::new();
            if let Err(err) = file.read_to_string(&mut src) {
                println!("Error while reading {}: {:?}", input, err);
                return;
            }

            if let Ok(data) = rock::codegen(&src, &mut rock::DebugLogger) {
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
            else {
                println!("COMPILATION FAILED!");
            }
        } else {
            println!("unable to open file: {}", input);
        }
    } else {
        println!("usage: rock <input file> <output file>");
    }
}
