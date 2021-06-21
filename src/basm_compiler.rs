use crate::basm_instructions::{BasmInstruction, basm_instruction_opcode, get_type_read_instruction, get_type_size, get_type_write_instruction, map_binary_op_instructions};
use crate::location::{fmt_loc, fmt_loc_err, FileNameLocations, Location};
use crate::parser::{AstBinaryOp, AstBlock, AstExpr, AstExprKind, AstFunCall, AstIfStatement, AstModule, AstProcDef, AstStatement, AstTop, AstTypes, AstVarAssign, AstVarDef, AstVarRead, AstWhileStatement, name_to_type, type_to_str};
use std::collections::HashMap;
use std::path::Path;

use std::fs::File;
use std::io::prelude::*;

const NATIVE_NAME_CAPACITY: usize = 256;

type BMword = i64;
type BMaddr = usize;

#[derive(Debug)]
pub struct CompiledExpr {
    pub loc: Location,
    pub inst_addr: BMaddr,
    pub expr_type: AstTypes,
}

#[derive(Debug)]
pub struct CompiledVar {
    pub def: AstVarDef,
    pub addr: BMaddr,
}

#[derive(Debug)]
pub struct CompiledProc {
    pub def: AstProcDef,
    pub addr: BMaddr,
}

#[derive(Debug)]
pub struct BinaryOpInstructions {
    pub inp_type :AstTypes,
    pub instr    :BasmInstruction,
    pub ret_type :AstTypes,
}

impl BinaryOpInstructions {
    pub fn new(inp_type :AstTypes, instr :BasmInstruction, ret_type :AstTypes ) -> Self {
        Self {inp_type, instr, ret_type }
    }
}


#[derive(Debug)]
pub struct BasmCompiler<'a> {
    program: Vec<i64>,
    memory: Vec<u8>,
    externals: HashMap<String, BMword>,
    global_vars: HashMap<String, CompiledVar>,
    procedures: HashMap<String, CompiledProc>,
    entry: BMaddr,
    filename_locations: &'a FileNameLocations,
}

impl<'a> BasmCompiler<'a> {
    pub fn new(filename_locations: &'a FileNameLocations) -> Self {
        Self {
            // memory : vec![0; 1024],
            program: Vec::new(),
            memory: Vec::new(),
            externals: HashMap::new(),
            global_vars: HashMap::new(),
            procedures: HashMap::new(),
            entry: 0,
            filename_locations,
        }
    }

    fn basm_push_inst(&mut self, instruction: BasmInstruction, operand: BMword) -> BMaddr {
        assert!((self.program.len() % 2) == 0);
        self.program.push(basm_instruction_opcode(instruction));
        self.program.push(operand);
        self.program.len() - 2
    }

    fn push_external_native(&mut self, native_name: String) -> BMword {
        let idx = self.externals.len();
        self.externals.insert(native_name, idx as BMword);
        idx as BMword
    }

    fn push_buffer_to_memory(&mut self, value: &[u8]) -> (BMword, BMword) {
        let idx = self.memory.len();
        for bt in value {
            self.memory.push(*bt);
        }
        (idx as BMword, (self.memory.len() - idx) as BMword)
    }

    fn push_string_to_memory(&mut self, value: &str) -> (BMword, BMword) {
        let idx = self.memory.len();

        for c in value.chars() {
            self.memory.push(c as u8);
        }
        (idx as BMword, (self.memory.len() - idx) as BMword)
    }

    fn check_function_arity(&self, func_call: &AstFunCall, arity: usize) {
        if func_call.args.len() == arity {
            return;
        }

        let loc_msg = fmt_loc_err(self.filename_locations, &func_call.loc);
        user_error!("{} Invalid number of function arguments for function {}, got {} arguments, expected {}",
            loc_msg,
            &func_call.name,
            func_call.args.len(),
            arity);
    }

    /// # Panics
    /// This function wil panic if the file can't be written to disk
    pub fn write_to_bm(&self, file_path: &Path) {

        const BM_FILE_MAGIC: u32 = 0x_A401_6D62;
        const BM_FILE_VERSION: u16 = 7;

        let mut file = File::create(file_path).unwrap();

        file.write_all(&BM_FILE_MAGIC.to_ne_bytes()).expect("write");
        file.write_all(&BM_FILE_VERSION.to_ne_bytes())
            .expect("write");

        // number of INSTRUCTIONS
        file.write_all(&(self.get_inst_addr()).to_ne_bytes())
            .expect("write");
        file.write_all(&(self.entry as u64).to_ne_bytes())
            .expect("write");

        let val = self.memory.len() as u64;
        file.write_all(&val.to_ne_bytes()).expect("write");

        let val = self.memory.len() as u64;
        file.write_all(&val.to_ne_bytes()).expect("write");

        let val = self.externals.len() as u64;
        file.write_all(&(val).to_ne_bytes()).expect("write");

        for x in &self.program {
            file.write_all(&x.to_ne_bytes()).expect("write");
        }

        file.write_all(&self.memory).expect("write");

        // each external native name is a fixed length string
        // with a maximum of 256 characters
        for external_native in self.externals.keys() {
            let mut chit = external_native.chars();
            for _ in 0..NATIVE_NAME_CAPACITY {
                let val = chit.next().map_or(0, |ch| ch as u8);
                let _ = file.write(&val.to_ne_bytes()).expect("write");
            }
        }

        drop(file);
    }


    fn compile_binary_op(&mut self, binary_op: &AstBinaryOp) -> AstTypes {
        let loc_msg = fmt_loc_err(self.filename_locations, &binary_op.loc);

        let compiled_lhs = self.compile_expr(binary_op.lhs.as_ref());
        let compiled_rhs = self.compile_expr(binary_op.rhs.as_ref());

        if compiled_lhs.expr_type != compiled_rhs.expr_type{
            user_error!(
                "{} binary operation {:?} not supported for LHS type {} with RHS type {}",
                loc_msg,
                binary_op.kind,
                type_to_str( compiled_lhs.expr_type),
                type_to_str( compiled_rhs.expr_type));
        }

        let bin_inst_opt = map_binary_op_instructions(compiled_lhs.expr_type,binary_op.kind);

        if let Some( ( basm_inst, result_type ))  = bin_inst_opt {
            self.basm_push_inst(basm_inst, 0);

            // return result type
            return result_type;
        } 

        user_error!(
            "{} binary operation {:?} not available for type: {}",
            loc_msg,
            binary_op.kind,
            type_to_str( compiled_lhs.expr_type));
    }

    fn compile_expr(&mut self, expr: &AstExpr) -> CompiledExpr {
        // println!("compile_expr {:?}", expr);

        let inst_addr = self.get_inst_addr();
        let mut expr_type = AstTypes::VOID;

        match &expr.kind {
            AstExprKind::FuncCall(func_call) => {
                // TODO only built in functions are supported at this point in time
                //println!("AstExprKind::FuncCall: {:?}", func_call);

                let mut func_idx: i64 = -1;
                if let Some(idx) = self.externals.get(&func_call.name) {
                    func_idx = *idx as i64;
                }

                if func_idx >= 0 {
                    // check arity
                    self.check_function_arity(&func_call, 1);

                    // compile the argument
                    self.compile_expr(&func_call.args[0]);

                    // println!("FUNC IDX : {:?}", func_idx);
                    // do function call
                    self.basm_push_inst(BasmInstruction::NATIVE, func_idx);
                } else if func_call.name == "ptr" {
                    expr_type = self.compile_ptr(func_call);
                } else if func_call.name == "write_ptr" {
                    expr_type = self.compile_write_ptr(func_call);
                } else if func_call.name == "cast" {
                    expr_type = self.compile_cast(func_call);
                } else if func_call.name == "load_ptr" {
                    expr_type = self.compile_load_ptr(func_call);
                } else if func_call.name == "store_ptr" {
                    expr_type = self.compile_store_ptr(func_call);
                } else if let Some(compiled_proc) = self.procedures.get(&func_call.name) {
                    let proc_addr = compiled_proc.addr as BMword;
                    self.basm_push_inst(BasmInstruction::CALL, proc_addr);
                } else {
                    let loc_msg = fmt_loc_err(self.filename_locations, &func_call.loc);
                    user_error!(
                        "{} Can't find definition for function name  {}",
                        loc_msg,
                        &func_call.name
                    );
                }
            }
            AstExprKind::LitString(literal) => {
                // AF TODO remove quotes?
                //println!("AstExprKind::LitString: {}", literal);
                expr_type = AstTypes::I64;

                let (mem_loc, mem_len) = self.push_string_to_memory(literal);

                self.basm_push_inst(BasmInstruction::PUSH, mem_loc);
                self.basm_push_inst(BasmInstruction::PUSH, mem_len);
            }
            AstExprKind::LitInt(value) => {
                expr_type = AstTypes::I64;
                self.basm_push_inst(BasmInstruction::PUSH, *value);
            }
            AstExprKind::LitBool(value) => {
                expr_type = AstTypes::BOOL;

                if *value {
                    self.basm_push_inst(BasmInstruction::PUSH, 1);
                } else {
                    self.basm_push_inst(BasmInstruction::PUSH, 0);
                }
            }
            AstExprKind::VarRead(value) => {
                expr_type = self.compile_var_read(value);
            }

            AstExprKind::BinarayOp(value) => {
                expr_type = self.compile_binary_op(value);
            }

            AstExprKind::LitFloat(_) |
            AstExprKind::LitChar(_) => todo!(),

        }

        CompiledExpr {
            loc: expr.loc,
            inst_addr,
            expr_type,
        }
    }

    fn compile_ptr(&mut self, func_call: &AstFunCall) -> AstTypes {
        self.check_function_arity(&func_call, 1);
        let arg = &func_call.args[0];
        if let AstExprKind::VarRead(var_read) = &arg.kind {
            if let Some(global_var) = self.global_vars.get(&var_read.name) {
                let var_addr = global_var.addr;

                self.basm_push_inst(BasmInstruction::PUSH, var_addr as BMword);
                AstTypes::PTR
            } else {
                let loc_msg = fmt_loc_err(self.filename_locations, &var_read.loc);
                user_error!("{} Can't read variable name {}", loc_msg, &var_read.name);
            }
        } else {
            let loc_msg = fmt_loc_err(self.filename_locations, &func_call.loc);
            user_error!(
                "{} Expected variable name of the argument of the ptr function",
                loc_msg
            );
        }
    }

    fn reinterpret_expr_as_type(&mut self, expr: &AstExpr) -> AstTypes {
        if let AstExprKind::VarRead(var_read_type) = &expr.kind {
            if let Some(ptr_type) = name_to_type(&var_read_type.name) {
                return ptr_type;
            }

            let loc_msg = fmt_loc_err(self.filename_locations, &expr.loc);
            user_error!("{} is not a valid type {}", loc_msg, &var_read_type.name);
        }
        let loc_msg = fmt_loc_err(self.filename_locations, &expr.loc);
        user_error!("{} Expected type name", loc_msg);
    }

    fn type_check_expr(&self, compiled_expr: &CompiledExpr, expected_type: AstTypes) {
        if compiled_expr.expr_type == expected_type {
            return;
        }
        let loc_msg = fmt_loc_err(self.filename_locations, &compiled_expr.loc);
        user_error!(
            "{} expected type {} but got type {}",
            loc_msg,
            type_to_str(expected_type),
            type_to_str(compiled_expr.expr_type)
        );
    }

    fn compile_write_ptr(&mut self, func_call: &AstFunCall) -> AstTypes {
        self.check_function_arity(&func_call, 2);

        let arg0 = &func_call.args[0];
        let arg1 = &func_call.args[1];

        // first put buffer pointer on stack
        let buffer = self.compile_expr(arg0);
        self.type_check_expr(&buffer, AstTypes::PTR);

        // then  put length on stack
        let length = self.compile_expr(arg1);
        self.type_check_expr(&length, AstTypes::I64);

        // call native write function
        if let Some(idx) = self.externals.get("write") {
            let func_idx = *idx as i64;
            self.basm_push_inst(BasmInstruction::NATIVE, func_idx);
        } else {
            unreachable!("expects native write function")
        }

        // no returns
        AstTypes::VOID
    }



    fn compile_cast(&mut self, func_call: &AstFunCall) -> AstTypes {
        self.check_function_arity(&func_call, 2);

        let arg0 = &func_call.args[0];
        let arg1 = &func_call.args[1];

        let to_type = self.reinterpret_expr_as_type(arg0);
        let from_expr = self.compile_expr(arg1);
        let from_type = from_expr.expr_type;

        if  ( from_type == AstTypes::I64 && to_type == AstTypes::PTR ) ||
            ( from_type == AstTypes::I64 && to_type == AstTypes::U8  ) ||
            ( from_type == AstTypes::U8  && to_type == AstTypes::PTR ) {
            return to_type;
        }

        let loc_msg = fmt_loc_err(self.filename_locations, &arg1.loc);
        user_error!("{} can't convert value of type {} to {}", 
            loc_msg, 
            type_to_str(from_type),
            type_to_str(to_type)
        );
    }

    fn compile_load_ptr(&mut self, func_call: &AstFunCall) -> AstTypes {
        self.check_function_arity(&func_call, 2);

        let arg0 = &func_call.args[0];
        let arg1 = &func_call.args[1];

        let compiled_ptr_arg = self.compile_expr(arg1);
        self.type_check_expr(&compiled_ptr_arg, AstTypes::PTR);
      
        let ptr_type = self.reinterpret_expr_as_type(arg0);
        let instr = get_type_read_instruction(ptr_type );

        if BasmInstruction::NOP == instr {
            let loc_msg = fmt_loc_err(self.filename_locations, &arg0.loc);
            user_error!("{} can't load {} type", loc_msg, type_to_str(ptr_type));
        }

        self.basm_push_inst(instr, 0);
        ptr_type
    }

    fn compile_store_ptr(&mut self, func_call: &AstFunCall) -> AstTypes {
        self.check_function_arity(&func_call, 3);


        let arg0 = &func_call.args[0];
        let arg1 = &func_call.args[1];
        let arg2 = &func_call.args[2];

        let ptr_type = self.reinterpret_expr_as_type(arg0);
        
        let ptr = self.compile_expr(arg1);       
        self.type_check_expr(&ptr, AstTypes::PTR);

        let val = self.compile_expr(arg2);
        self.type_check_expr(&val, ptr_type);

        let instr = get_type_write_instruction(ptr_type );

        if BasmInstruction::NOP == instr {
            let loc_msg = fmt_loc_err(self.filename_locations, &arg0.loc);
            user_error!("{} can't store {} type", loc_msg, type_to_str(ptr_type));
        }

        self.basm_push_inst(instr, 0); 

        // result of store is void
        AstTypes::VOID
    }

    fn compile_var_read(&mut self, var_read: &AstVarRead) -> AstTypes {

        //println!("compile_var_read ");
        if let Some(global_var) = self.global_vars.get(&var_read.name) {
            let var_type = global_var.def.var_type;
            let var_addr = global_var.addr;

            let instr = get_type_read_instruction(var_type );

            if BasmInstruction::NOP == instr {
                let loc_msg = fmt_loc_err(self.filename_locations, &var_read.loc);
                user_error!("{} read var not supported for type {}", loc_msg, type_to_str(var_type));
            }

            self.basm_push_inst(BasmInstruction::PUSH, var_addr as BMword);
            self.basm_push_inst(instr, 0);
            return var_type;

        }

        let loc_msg = fmt_loc_err(self.filename_locations, &var_read.loc);
        user_error!("{} Can't read variable name {}", loc_msg, &var_read.name);
    }

    fn compile_if_statment(&mut self, if_statement: &AstIfStatement) {
        println!("Compile if instruction condition ");
        let compiled_cond = self.compile_expr(&if_statement.condition);

        let loc_msg = fmt_loc_err(self.filename_locations, &if_statement.loc);
        if compiled_cond.expr_type == AstTypes::VOID {
            user_error!(
                "{} condition can't be of type {:?} in the if-else statement",
                loc_msg,
                compiled_cond.expr_type
            );
        }

        self.basm_push_inst(BasmInstruction::NOT, 0);
        let jmp_no_if_op = self.basm_push_inst(BasmInstruction::JMPIf, 0) + 1;
        //println!("######### jmp_not_if_addr {} ", jmp_no_if_op);

        self.compile_block(&if_statement.then_block);

        if let Some(else_block) = &if_statement.else_block {
            // add instruction to jump over else block
            let jmp_over_else = self.basm_push_inst(BasmInstruction::JMP, 0) + 1;

            // fill in deferred address of no_if block
            let jmp_addr = self.get_inst_addr() as BMword;
            self.program[jmp_no_if_op] = jmp_addr;

            self.compile_block(&else_block);

            // fill in deferred address of jmp over else block
            let jmp_addr = self.get_inst_addr() as BMword;
            self.program[jmp_over_else] = jmp_addr;
        } else {
            // fill in deferred address of no_if block
            let jmp_addr = self.get_inst_addr() as BMword;
            self.program[jmp_no_if_op] = jmp_addr;
        }
    }

    fn compile_while_statement(&mut self, while_statement: &AstWhileStatement) {
        //println!("Compile while instruction condition ");
        let compiled_cond = self.compile_expr(&while_statement.condition);

        let loc_msg = fmt_loc_err(self.filename_locations, &while_statement.loc);
        if compiled_cond.expr_type == AstTypes::VOID {
            user_error!(
                "{} condition can't be of type {:?} in the while statement",
                loc_msg,
                compiled_cond.expr_type
            );
        }

        self.basm_push_inst(BasmInstruction::NOT, 0);
        let end_while_jmp = self.basm_push_inst(BasmInstruction::JMPIf, 0) + 1;

        self.compile_block(&while_statement.block);

        self.basm_push_inst(BasmInstruction::JMP, compiled_cond.inst_addr as BMword);
        let end_while_addr = self.get_inst_addr() as BMword;

        self.program[end_while_jmp] = end_while_addr;
    }

    fn compile_var_assign(&mut self, var_assign: &AstVarAssign) {
        //println!("compile_var_assign ");
        if let Some(global_var) = self.global_vars.get(&var_assign.name) {
            let var_addr = global_var.addr as BMword;
            let var_type = global_var.def.var_type;

            self.basm_push_inst(BasmInstruction::PUSH, var_addr);

            let compiled_expr = self.compile_expr(&var_assign.expr);

            if compiled_expr.expr_type != var_type {
                let loc_msg = fmt_loc_err(self.filename_locations, &var_assign.loc);

                user_error!(
                    "{} Can't assign value of type {:?} to variable {} of type {:?}",
                    loc_msg,
                    compiled_expr.expr_type,
                    &var_assign.name,
                    var_type
                );
            }

            // get instruction based on variable assignment type
            let instr = get_type_write_instruction(var_type );

            if BasmInstruction::NOP == instr {
                let loc_msg = fmt_loc_err(self.filename_locations, &var_assign.loc);
                user_error!("{} can't assign to type {}", loc_msg, type_to_str(var_type));
            }
    
            self.basm_push_inst(instr, 0); 

            return;
        }

        let loc_msg = fmt_loc_err(self.filename_locations, &var_assign.loc);
        user_error!(
            "{} Can't find address for variable name {}",
            loc_msg,
            &var_assign.name
        );
    }

    pub fn compile_statement(&mut self, stmt: &AstStatement) {
        //println!("compile_statement: {:?}", &stmt);
        match &stmt {
            AstStatement::Expr(expr) => {
                let compiled_expr = self.compile_expr(&expr);
                if compiled_expr.expr_type != AstTypes::VOID {
                    // drop if expression type is non void
                    self.basm_push_inst(BasmInstruction::DROP, 0);
                }
            }
            AstStatement::If(if_statement) => {
                self.compile_if_statment(&if_statement);
            }
            AstStatement::VarAssign(var_assign) => {
                self.compile_var_assign(var_assign);
            }
            AstStatement::While(while_stmt) => {
                self.compile_while_statement(while_stmt);
            }
        }
    }

    pub fn compile_block(&mut self, block: &AstBlock) {
        //println!("compile_block ");
        let stmts = &block.statements;

        for stmt in stmts {
            self.compile_statement(&stmt)
        }
    }

    fn get_inst_addr(&self) -> BMaddr {
        self.program.len() / 2
    }

    fn compile_proc_def(&mut self, proc_def: &AstProcDef) {
        //println!("compile_proc_def ");
        let inst_addr = self.get_inst_addr();
        let name = proc_def.name.clone();
        let body = &proc_def.body;

        // check if name already exist
        if let Some(existing_proc) = self.procedures.get(&name) {
            let loc_msg = fmt_loc_err(self.filename_locations, &proc_def.loc);
            user_error!(
                "{} procedure with name {} is already defined at {}",
                loc_msg,
                &name,
                fmt_loc(self.filename_locations, &existing_proc.def.loc)
            );
        }

        // insert before the block, so we can do recursion!
        self.procedures.insert(
            name,
            CompiledProc {
                def: proc_def.clone(),
                addr: inst_addr,
            },
        );

        self.compile_block(&body);
        self.basm_push_inst(BasmInstruction::RET, 0);
    }

    fn compile_var_def(&mut self, var_def: &AstVarDef) {
        //println!("compile_var_def ");


        let var_size = get_type_size(var_def.var_type);

        if var_size == 0 {
            let loc_msg = fmt_loc_err(self.filename_locations, &var_def.loc);
            user_error!(
                "{} definining a variable of type {} is not allowed",
                loc_msg,
                type_to_str(var_def.var_type)
            );
        }

        // check if var already exists
        if let Some(existing_var) = self.global_vars.get(&var_def.name) {
            let existing_loc_msg = fmt_loc_err(self.filename_locations, &existing_var.def.loc);
            let loc_msg = fmt_loc_err(self.filename_locations, &var_def.loc);
            user_error!(
                "{} variable with name {} is previously defined overhere {}",
                loc_msg,
                &var_def.name,
                existing_loc_msg);            
        }

        let zero_vec = vec![0; var_size];
        let (addr, _) = self.push_buffer_to_memory(&zero_vec);

        #[allow(clippy::cast_possible_truncation)]
        #[allow(clippy::cast_sign_loss)]
        let addr = addr as BMaddr;

        self.global_vars.insert(
            var_def.name.clone(),
            CompiledVar {
                addr,
                def: var_def.clone(),
            },
        );

    }

    fn generate_heap_base(&mut self, heap_base_name : &str ) {
        if let Some(var ) = self.global_vars.get(heap_base_name) {
            if var.def.var_type != AstTypes::PTR {
                let loc_msg = fmt_loc_err(self.filename_locations, &var.def.loc);
                user_error!(
                    "{} Heap base variable named {} needs to be of type  type {} but got type {}",
                    loc_msg,
                    &heap_base_name,
                    type_to_str(AstTypes::PTR),
                    type_to_str(var.def.var_type)
                );                
            }

            // end of memory location into heap_base variable address
            let addr_bts = self.memory.len().to_ne_bytes();
            self.memory[ var.addr .. var.addr + addr_bts.len()].clone_from_slice(&addr_bts);
        }
    }


    fn generate_entry_point(&mut self, entry_name: &str) {
        if let Some(entry_proc) = self.procedures.get(entry_name) {
            // set entry point to this startup code
            self.entry = self.get_inst_addr();

            // create startup code by making call to entry function, halt on return
            let entry_proc_addr = entry_proc.addr as BMword;
            self.basm_push_inst(BasmInstruction::CALL, entry_proc_addr);
            self.basm_push_inst(BasmInstruction::HALT, 0);
        } else {
            let loc = Location {
                row: 0,
                col: 0,
                file_idx: 0,
            };
            let loc_msg = fmt_loc_err(self.filename_locations, &loc);
            user_error!("{} can't find entry procedure with {}", loc_msg, entry_name);
        }
    }

    pub fn compile(&mut self, module: &AstModule, entry_name: &str) {
        // insert native write function
        self.push_external_native("write".to_string());

        for top in &module.tops {
            match top {
                AstTop::ProcDef(proc_def) => {
                    self.compile_proc_def(&proc_def);
                }
                AstTop::VarDef(var_def) => {
                    self.compile_var_def(&var_def);
                }
            }
        }

        self.generate_heap_base("heap_base");
        self.generate_entry_point(entry_name);
    }
}
