extern crate boulder;

use std::env;
use std::fs::File;
use std::io::{Read, Write};

const USAGE: &str = "usage: boulder <input file> [<output file>]";

pub fn main() {
    let mut args = env::args().skip(1);
    if let Some(ref input) = args.next() {
        let output = args.next();
        let output = output.as_ref().map(|t| &**t).unwrap_or("./a.data");
        if let Some(_) = args.next() {
            eprintln!("{}", USAGE);
            return;
        }

        if let Ok(mut file) = File::open(input) {
            let mut src = String::new();
            if let Err(err) = file.read_to_string(&mut src) {
                eprintln!("Error while reading {}: {:?}", input, err);
            }

            if let Ok(res) = boulder::compile(&src) {
                if let Ok(mut file) = File::create(output) {
                    write!(file, "{}", res).unwrap();
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
}
