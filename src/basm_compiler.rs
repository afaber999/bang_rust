use std::collections::HashMap;
use std::path::{Path};
use crate::location::{FileNameLocations, fmt_loc_err};
use crate::parser::{AstBlock, AstExpr, AstFunCall, AstIfStatement, AstProcDef, AstStatement};
use crate::basm_instructions::{BasmInstruction, basm_instruction_opcode};

use std::fs::File;
use std::io::prelude::*;

const NATIVE_NAME_CAPACITY : usize =  256;

type BMword = i64;
type BMaddr = usize;

#[derive(Debug)]
pub struct BasmCompiler<'a> {
    program             : Vec<i64>,
    memory              : Vec<u8>,
    externals           : HashMap<String, BMword>,
    entry               : BMaddr,
    filename_locations  : &'a FileNameLocations,
}

impl<'a> BasmCompiler<'a> {
    pub fn new(filename_locations : &'a FileNameLocations,) ->Self {
        Self {
            // memory : vec![0; 1024],
            program     : Vec::new(),
            memory      : Vec::new(),
            externals   : HashMap::new(),
            entry       : 0,
            filename_locations,            
        }
    }
    
    fn basm_push_inst(&mut self, instruction: &BasmInstruction, operand: BMword ) -> BMaddr
    {
        assert!( (self.program.len() %2) == 0);
        self.program.push( basm_instruction_opcode(instruction) );
        self.program.push( operand );
        self.program.len() - 2
    }

    fn push_external_native(&mut self, native_name: String) -> BMword
    {
        let idx = self.externals.len();
        self.externals.insert(native_name, idx as BMword);
        idx as BMword
    }

    fn push_string_to_memory(&mut self, value : &str) -> (BMword, BMword)
    {
        let idx = self.memory.len();

        for c in value.chars() {
            self.memory.push( c as u8);
        }
        (idx as BMword, (self.memory.len() - idx) as BMword)
    }

    fn check_function_arity(&self, func_call: &AstFunCall,  arity: usize) {

        if func_call.args.len() == arity {
            return;
        }

        let loc_msg = fmt_loc_err( self.filename_locations, &func_call.loc);
        user_error!("{} Invalid number of function arguments for function {}, got {} arguments, expected {}",
            loc_msg,
            &func_call.name,
            func_call.args.len(),
            arity);
    }



    pub fn write_to_bm(&self, file_path :&Path) {

        const BM_FILE_MAGIC  : u32 = 0xa4016d62;
        const BM_FILE_VERSION: u16 = 7;

        let mut file = File::create(file_path).unwrap();

        file.write(&BM_FILE_MAGIC.to_ne_bytes()).expect("write");
        file.write(&BM_FILE_VERSION.to_ne_bytes()).expect("write");

        // number of INSTRUCTIONS
        let val  = self.program.len() as u64 / 2;
        file.write(&val.to_ne_bytes()).expect("write");

        let val  = self.entry as u64;
        file.write(&val.to_ne_bytes()).expect("write");

        let val  = self.memory.len() as u64;
        file.write(&val.to_ne_bytes()).expect("write");

        let val  = self.memory.len() as u64;
        file.write(&val.to_ne_bytes()).expect("write");

        let val  = self.externals.len() as u64;
        file.write(&val.to_ne_bytes()).expect("write");

        for x in &self.program {
            file.write(&x.to_ne_bytes()).expect("write");
        } 

        file.write(&self.memory).expect("write");

        // each external native name is a fixed length string
        // with a maximum of 256 characters
        for external_native in self.externals.keys() {
            let mut chit = external_native.chars();
            for _ in 0..NATIVE_NAME_CAPACITY {
                let val = if let Some(ch) = chit.next() {
                    ch as u8
                } else {0};
                file.write(&val.to_ne_bytes()).expect("write");
            }
        }

        drop(file);     
    }


    fn compile_expr(&mut self, expr: &AstExpr) {

        println!("compile_expr {:?}", expr);

        match &expr {
            AstExpr::FuncCall(func_call) => {
                // TODO only built in functions are supported at this point in time
                println!("AstExprKind::FuncCall: {:?}", func_call);

                if func_call.name == "true" {
                    self.basm_push_inst(&BasmInstruction::PUSH, 1);
                }
                else if func_call.name == "false" {
                    self.basm_push_inst(&BasmInstruction::PUSH, 0);
                } else {
                    let mut func_idx : i64 = -1;
                    if let Some(idx)= self.externals.get(&func_call.name) {
                        func_idx = *idx as i64;
                    }
    
                    if func_idx >= 0 {
    
                        // check arity
                        self.check_function_arity(&func_call, 1); 
    
                        // compile the argument
                        self.compile_expr(&func_call.args[0]);
    
                        println!("FUNC IDX : {:?}", func_idx);
                        // do function call
                        self.basm_push_inst(&BasmInstruction::NATIVE, func_idx);                    
                    } else {
                        user_error!("Only native functions are supported");
                    }    
                }
            },
            AstExpr::LitString(literal) => {
                // AF TODO remove quotes?
                println!("AstExprKind::LitString: {}", literal);
                let (mem_loc, mem_len) = self.push_string_to_memory(literal);
        
                self.basm_push_inst(&BasmInstruction::PUSH, mem_loc);
                self.basm_push_inst(&BasmInstruction::PUSH, mem_len);                
            },
            AstExpr::LitFloat(_) => todo!(),
            AstExpr::LitInt(_) => todo!(),
            AstExpr::LitChar(_) => todo!(),

        }
    }

    fn compile_if_statment(&mut self, if_statement: &AstIfStatement) {

        println!("Compile if instruction condition ");
        self.compile_expr(&if_statement.condition);
        self.basm_push_inst(&BasmInstruction::NOT, 0);
        let jmp_if_addr = self.basm_push_inst(&BasmInstruction::JMPIf, 0);
        println!("######### jmp_if_addr {} ",jmp_if_addr);


        self.compile_block(&if_statement.then_block);
        let body_end_addr = self.program.len() / 2;
        println!("######### body_end_addr {} ",body_end_addr);

        self.program[jmp_if_addr + 1 ] = body_end_addr as BMword;
    }

    pub fn compile_statement(&mut self, stmt: &AstStatement) {
        println!("compile_statement: {:?}", &stmt);
        match &stmt {
            AstStatement::Expr(expr) => {
                self.compile_expr(&expr)
            },
            AstStatement::If(if_statement) => {
                self.compile_if_statment( &if_statement);
            }
        }
    }

    pub fn compile_block(&mut self, block: &AstBlock ) {
        println!("compile_block ");
        let stmts = &block.statements;

        for stmt in stmts {
            self.compile_statement(&stmt)
        }
    }

    fn compile_proc_def(&mut self, proc_def :&AstProcDef ) {

        println!("compile_proc_def ");
        let body = &proc_def.body;
        self.compile_block( &body );

    }

    pub fn compile(&mut self, proc_def :&AstProcDef ) {
        // insert native write function
        self.push_external_native( "write".to_string() );
        self.compile_proc_def(proc_def);


        // let (mem_loc, mem_len) = self.push_string_to_memory("Hello, World!");
        // println!("Memory size: {}", self.memory.len());

        // self.basm_push_inst(&BasmInstruction::PUSH, mem_loc);
        // self.basm_push_inst(&BasmInstruction::PUSH, mem_len);
        // self.basm_push_inst(&BasmInstruction::NATIVE, write_loc);
        self.basm_push_inst(&BasmInstruction::HALT, 0);

    }

}
