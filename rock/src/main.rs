extern crate rock;

use std::env;
use std::fs::File;
use std::io::{Read, Write};

use rock::TokenIter;

pub fn main() {
    if let [ref input, ref output] = *env::args().skip(1).collect::<Box<_>>() {
        if let Ok(mut file) = File::open(input) {
            let mut src = String::new();
            if let Err(err) = file.read_to_string(&mut src) {
                println!("Error while reading {}: {:?}", input, err);
                return;
            }

            let blocks = rock::parse(&src, &mut rock::DebugLogger);

            if let Ok(mut file) = File::create(output) {
                if let Err(err) = writeln!(file, "{:?}", blocks) {
                    println!("Error while writing to {}: {:?}", input, err);
                    return;
                }
            }
        } else {

        }
    } else {
        println!("usage: rock <input file> <output file>");
    }
}
