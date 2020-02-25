use std::{env, process};

fn invalid(code: &str) -> ! {
    eprintln!("mirco: `{}` is not a valid opcode", code);
    process::exit(1);
}

fn hexdigit(digit: char, code: &str) -> u8 {
    if digit.is_ascii() {
        let digit = digit.to_ascii_lowercase() as u8;
        if (b'a'..=b'f').contains(&digit) {
            digit as u8 - b'a' + 10
        } else if (b'0'..=b'9').contains(&digit) {
            digit as u8 - b'0'
        } else {
            invalid(code);
        }
    } else {
        invalid(code);
    }
}

fn main() {
    let mut cont = false;
    for input in env::args().skip(1) {
        if cont {
            println!();
        } else {
            cont = true;
        }
        let digits: Box<[_]> = input.chars().map(|c| hexdigit(c, &input)).collect();
        if digits.len() != 4 {
            invalid(&input);
        }

        digit_0_3(digits[3], digits[1]);
        digit_4_7(digits[2], (digits[0] & 0b1100) >> 2 == 0b01);
        digit_12_15(digits[0], digits[1]);
    }
}

fn digit_0_3(digit: u8, alu: u8) {
    if digit & 1 != 0 {
        println!("Increment P");
    }

    println!(
        "Bus input: {}",
        match digit >> 1 {
            0b000 => "RAM",
            0b001 => match alu {
                0b0000 => "A + B",
                0b0001 => "A - B",
                0b0010 => "A & B",
                0b0011 => "A | B",
                0b0100 => "A ^ B",
                0b0101 => "A << B",
                0b0110 => "A >> B",
                0b0111 => "A",
                0b1111 => "!A",
                _ => "UNDEFINED",
            },
            0b100 => "A",
            0b101 => "B",
            0b110 => "C",
            0b111 => "D",
            _ => "UNDEFINED",
        }
    );
}

fn digit_4_7(digit: u8, update_register: bool) {
    if update_register {
        println!(
            "Update register: {}",
            match digit & 0b0111 {
                0b000 => "A",
                0b001 => "B",
                0b010 => "C",
                0b011 => "D",
                0b100 => "M1",
                0b101 => "M2",
                0b110 => "P1",
                0b111 => "P2",
                _ => unreachable!(),
            }
        );
    } else if digit & 0b0111 != 0 {
        eprintln!("UPDATE REGISTER IS NOT ACTIVE, CONSIDER SETTING IT TO 0");
    }

    if digit & 0b1000 != 0 {
        println!("Reset P1");
    }
}

fn digit_12_15(digit: u8, alu: u8) {
    if digit & 0b0001 != 0 {
        println!("Update RAM");
    }

    println!(
        "RAM select: {}",
        if digit & 0b0010 != 0 { 'M' } else { 'P' }
    );

    if digit & 0b1000 != 0 {
        println!(
            "Update IR: {}",
            if digit & 0b0100 == 0 {
                // on carry
                match alu {
                    0b0001 => "A < B",
                    0b0010 => "A > B",
                    0b1001 => "A >= B",
                    0b1010 => "A <= B",
                    _ => "UNDEFINED",
                }
            } else {
                // on zero
                match alu {
                    0b0001 => "A == B",
                    0b0111 => "A == 0",
                    0b1001 => "A != B",
                    0b1111 => "A != 0",
                    _ => "UNDEFINED",
                }
            }
        );
    }
}
