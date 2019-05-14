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

            let token = TokenIter::new(&src);

            if let Ok(mut file) = File::create(output) {
                for tok in token {
                    if let Err(err) = writeln!(file, "{:?}", tok.content(&src)) {
                        println!("Error while writing to {}: {:?}", input, err);
                        return;
                    }
                }
            }
        } else {

        }
    } else {
        println!("usage: rock <input file> <output file>");
    }
}
