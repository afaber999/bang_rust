use anyhow::{Result};
use std::{convert::From, env, panic, path::PathBuf};

#[macro_use]
pub mod errors;


pub mod lexer;
pub mod location;
pub mod parser;
pub mod token;
pub mod basm_compiler;
pub mod basm_instructions;

use lexer::Lexer;
use location::FileNameLocations;
use parser::Parser;
use basm_compiler::BasmCompiler;

fn usage(program_name: &str) {
    println!("Usage: {} [OPTIONS] <input.bang>", program_name);
    println!("OPTIONS:");
    println!("    -o <output>                       Provide output path");
    println!("    -h                                Print this help to stdout");
}


fn main() -> Result<()> {
    panic::set_hook(Box::new(|panic_info| {
        
        if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            let msg = s.to_string();
            eprintln!("{:?}", msg);
        } else {

            eprintln!("{}", &panic_info);
        }
        std::process::exit(-1);
    }));

    let filename_locations = FileNameLocations::new();

    let mut arg_it = env::args();
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
    let input_file_name = input_file_path.to_str().unwrap().to_string();

    let input_file = std::fs::read_to_string(input_file_path)?;
    print!( "{}", input_file);
    println!("----------------------------------------------");
    // for line in reader.lines() {
    //     println!("{}", line?);

    // }
    let lexer = Lexer::new(input_file, input_file_name, &filename_locations);
    let mut parser = Parser::new(lexer, &filename_locations);
    let module = parser.parse();

    let mut basm_compiler = BasmCompiler::new(&filename_locations);

    basm_compiler.compile( &module, "main");
    basm_compiler.write_to_bm(&output_file_path);

    //basm_compiler.save(&output_file_path);


    println!("Done");
    Ok(())
}
