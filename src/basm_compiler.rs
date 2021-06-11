use std::collections::HashMap;
use std::path::{Path, PathBuf};
use crate::parser::{AstBlock, AstExpr, AstExprKind, AstProcDef};
use crate::basm_instructions::{BasmInstruction, basm_instruction_opcode};

use std::fs::File;
use std::io::prelude::*;

const NATIVE_NAME_CAPACITY : usize =  256;

type BMword = i64;
type BMaddr = usize;

#[derive(Debug, Default)]
pub struct BasmCompiler {
    program     : Vec<i64>,
    memory      : Vec<u8>,
    externals   : HashMap<String, BMword>,
    entry       : BMaddr,
}

impl BasmCompiler {
    pub fn new() ->Self {
        Self {
            // memory : vec![0; 1024],
            program     : Vec::new(),
            memory      : Vec::new(),
            externals   : HashMap::new(),
            entry       : 0,
        }
    }
    
    fn basm_push_inst(&mut self, instruction: &BasmInstruction, operand: BMword ) -> BMaddr
    {
        assert!( (self.program.len() %2) == 0);
        self.program.push( basm_instruction_opcode(instruction) );
        self.program.push( operand );
        self.program.len()
    }

    fn push_external_native(&mut self, native_name: String) -> BMword
    {
        let idx = self.externals.len();
        self.externals.insert(native_name, idx as BMword);
        idx as BMword
    }

    fn push_string_to_memory(&mut self, value : String) -> (BMword, BMword)
    {
        let idx = self.memory.len();

        for c in value.chars() {
            self.memory.push( c as u8);
        }
        (idx as BMword, (self.memory.len() - idx) as BMword)
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


        // // Write a slice of bytes to the file
        // file.write_all(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15])?;

        // // write meta

        // PACK(struct Bm_File_Meta {
        //     uint32_t magic;
        //     uint16_t version;
        //     uint64_t program_size;
        //     uint64_t entry;
        //     uint64_t memory_size;
        //     uint64_t memory_capacity;
        //     uint64_t externals_size;
        // });        
    }
    fn compile_expr(&mut self, expr: &AstExpr) {
        match &expr.kind {
            AstExprKind::Empty => todo!(),
            AstExprKind::FuncCall(args) => 
                if args.name == "write" {
                    todo!()
                } else {
                    user_error!("TODO Unknown function");
                }
            AstExprKind::LitFloat(_) => todo!(),
            AstExprKind::LitInt(_) => todo!(),
            AstExprKind::LitChar(_) => todo!(),
            AstExprKind::LitString(_) => {
                //let str_adr = basm_push_string_to_memory
                //basm_push_inst(basm, INST_PUSH, str_adr )
                todo!()
            },
        }
    }


    pub fn compile_block(&mut self, block: &AstBlock ) {
        let stmts = &block.statements;

        for stmt in stmts {
            self.compile_expr(&stmt.expr);
        }
    }

    fn compile_proc_def(&mut self, proc_def :&AstProcDef ) {

        // basm_push_external_native(&basm, "write");

        let body = &proc_def.body;
        self.compile_block( &body );

    }

    pub fn compile(&mut self, _proc_def :&AstProcDef ) {


        // basm_push_external_native(&basm, "write");
        //self.compile_proc_def(proc_def);

        // insert native write function
        let write_loc = self.push_external_native( "write".to_string() );

        let (mem_loc, mem_len) = self.push_string_to_memory("Hello, World\n".to_string());
        println!("Memory size: {}", self.memory.len());

        self.basm_push_inst(&BasmInstruction::PUSH, mem_loc);
        self.basm_push_inst(&BasmInstruction::PUSH, mem_len);
        self.basm_push_inst(&BasmInstruction::NATIVE, write_loc);
        self.basm_push_inst(&BasmInstruction::HALT, 0);

    }

}
