use anyhow::{Result};
use std::{env, path::PathBuf};
use std::convert::From;

pub mod lexer;
pub mod location;

use lexer::Lexer;
use token::token_type_name;
use location::FileNameLocations;

use crate::location::fmt_loc_err;

pub mod token;

fn usage(program_name: &str) {
    println!("Usage: {} [OPTIONS] <input.bang>", program_name);
    println!("OPTIONS:");
    println!("    -o <output>                       Provide output path");
    println!("    -h                                Print this help to stdout");
}

fn main() -> Result<()> {

    let filename_locations = FileNameLocations::new();

    let mut arg_it = env::args().into_iter();
    let program_name = arg_it.next().expect("first argument");
    let mut input_file_path = PathBuf::default();
    let mut output_file_path = PathBuf::default();

    while let Some(arg) = arg_it.next() {
        match arg.as_ref() {
            "-o" => {
                if let Some(name) = arg_it.next() {
                    output_file_path = PathBuf::from(name);
                }
                else {
                    usage(&program_name);
                    eprintln!("ERROR: no value is provided for flag -o ");
                    return Ok(())
                }
            },
            "-h" => usage(&program_name),
            _ => input_file_path = PathBuf::from(arg),
        }
    }

    if input_file_path == PathBuf::default() {
        usage(&program_name);
        eprintln!("ERROR: no name for input file is ");
        return Ok(())
    }

    if output_file_path == PathBuf::default() {
        output_file_path = input_file_path.clone();
        output_file_path.set_extension("bm");
    }

    println!("Input filename : {:?}", input_file_path.as_os_str());
    println!("Output filename: {:?}", output_file_path.as_os_str());

    let input_file = std::fs::read_to_string(input_file_path)?;
    print!( "{}", input_file);
    println!("----------------------------------------------");
    // for line in reader.lines() {
    //     println!("{}", line?);

    // }
    let todofix = "file".to_string();
    let idx =  filename_locations.insert(todofix);    

    let mut tokenizer = Lexer::new(input_file, idx);

    while let Some(nt) = tokenizer.next() {

        // {
        //     let tok_str = tokenizer.get_string( nt.text_start, nt.text_len );
        //     println!("next line row:{} indx: {} :{}:", &nt.loc.row, &nt.loc.col, tok_str );
        // }
        let tok_str = tokenizer.get_string( nt.text_start, nt.text_len );
        let tok_type_name = token_type_name(nt.token_type);

        let err_str = fmt_loc_err(&filename_locations, &nt.loc);
        println!("TOKEN {} {}  :{}:", &tok_type_name, &err_str, &tok_str );
       // tokenizer.dump();
        //tokenizer.next_line();
    }
    
    println!("Done");
    Ok(())
}
