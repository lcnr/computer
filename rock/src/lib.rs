use std::io::{self, BufRead, Read};

pub enum Command {
    Idle,
}

struct ParseError;

#[derive(Debug, Clone)]
pub struct Error;

#[derive(Debug, Clone)]
pub struct Warning;

impl Command {
    fn parse<L: Logger>(s: &str, logger: &mut L) -> Result<Self, ParseError> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum GenerateAsmError {
    IoError(io::Error),
    InvalidAsm
}

impl From<io::Error> for GenerateAsmError {
    fn from(err: io::Error) -> Self {
        GenerateAsmError::IoError(err)
    }
}

pub trait Logger {
    fn log_error(&mut self, err: Error);

    fn log_warning(&mut self, warning: Warning);
}

pub struct DebugLogger;

impl Logger for DebugLogger {
    fn log_error(&mut self, err: Error) {
        println!("ERROR: {:?}", err);
    }

    fn log_warning(&mut self, warning: Warning) {
        println!("WARN: {:?}", warning);
    }
}

pub fn generate_asm<R: BufRead, L: Logger>(src: R, logger: &mut L) -> Result<Vec<u8>, GenerateAsmError> {
    let mut commands = Vec::new();
    let mut success = true;

    for line in src.lines() {
        let line = line?;
        if let Ok(command) = Command::parse(&line, logger) {
            commands.push(command);
        } else {
            success = false;
        }
    }


    let mut data = Vec::new();
    for command in commands.iter() {
        match command {
            Command::Idle => data.extend_from_slice(b"00"),
        }
    }


    if success {
        Ok(data)
    } else {
        Err(GenerateAsmError::InvalidAsm)
    }
}
