extern crate rock;

use std::env;
use std::fs::File;
use std::io::{Write, BufReader};

fn main() {
    let mut error_count = 0;
    let mut warning_count = 0;

    let mut failed = Vec::new();

    for file_name in env::args().skip(1) {
        macro_rules! error {
            ($($arg:tt)*) => {
                {
                    error_count += 1;
                    print!("ERROR: ");
                    println!($($arg)*);
                    failed.push(file_name);
                    continue
                }
            };
        }

        macro_rules! warn {
            ($($arg:tt)*) => {
                {
                    warning_count += 1;
                    print!("WARNING: ");
                    println!($($arg)*);
                }
            };
        }

        println!("now compiling `{}`", file_name);
        let raw_name: &str = match file_name.rfind('.') {
            Some(idx) => {
                if &file_name[idx..] != ".ra" {
                    warn!(
                        "unexpected file type: {}. expected a `*.ra` file",
                        &file_name[idx..]
                    );
                }

                &file_name[..idx]
            }
            None => {
                warn!(
                    "unexpected file name: {}, expected a `*.ra` file",
                    file_name
                );
                &file_name
            }
        };

        let file = if let Ok(file) = File::open(&file_name) {
            file
        } else {
            error!(
                "`{}` can not be opened.\nusage: rock (<file_name>)*",
                file_name
            );
        };

        match rock::generate_asm(BufReader::new(file), &mut rock::DebugLogger) {
            Ok(asm) => {
                let file_name = format!("{}.data", raw_name);
                if let Ok(mut file) = File::create(&file_name) {
                    if file.write(&asm).is_err() {
                        error!("failed writing to file `{}`", file_name);
                    }
                } else {
                    error!("failed opening file `{}`", file_name);
                }
            }
            Err(err) => error!("failed to compile `{}`", file_name),
        }
        println!("successfully compiled `{}`\n", file_name);
    }
}
