use crate::{lexer::Lexer, location::{FileNameLocations, Location, fmt_loc_err}, token::{TokenKind, token_kind_name}};


#[derive(Debug)]
pub struct AstIfStatement {
    condition  : AstExpr,
    then_block : Box<AstStatement>,
    else_block : Box<AstStatement>, 
}

#[derive(Debug)]
pub enum AstStatement {
    Expr( AstExpr),
    If( AstIfStatement ),
}

#[derive(Debug)]
pub struct AstFunCall {
    pub loc  : Location,
    pub name : String,
    pub args : Vec<AstExpr>,
}

#[derive(Debug)]
pub enum AstExpr {
    FuncCall(AstFunCall),
    LitFloat(f64),
    LitInt(i64),
    LitChar(char),
    LitString(String),
}

#[derive(Debug)]
pub struct AstBlock{
    pub statements : Vec<AstStatement>,
} 

#[derive(Debug)]
pub struct AstProcDef {
    pub name : String,
    pub body : AstBlock,
} 

#[derive(Debug)]
pub struct Parser<'a> {
    lexer : Lexer<'a>,
    filename_locations : &'a FileNameLocations,
}

impl<'a> Parser<'a> {
    pub fn new(lexer : Lexer<'a>, filename_locations : &'a FileNameLocations) -> Self {
        Self {
            lexer,
            filename_locations,
        }
    }


    fn parse_string_literal(&mut self) -> String {

        println!("parse_string_literal");

        let token = self.lexer.expect_token_next(TokenKind::Literal);

        let mut i = token.text_start + 1;
        let last_index = i + token.text_len-2;
        
        let mut literal_chars = Vec::new();

        while i < last_index   {
            let ch = self.lexer.get_char(i);
            if ch == '\\' {
                if i + 1 >= last_index {
                    let mut loc = token.loc;
                    loc.col += i + 1;
                    let loc_msg = fmt_loc_err( self.filename_locations, &token.loc);
                    user_error!("{} unfinished string literal escape sequence", loc_msg);
                }
    
                let ch = self.lexer.get_char(i+1); 
                match ch {
                        '0' => { literal_chars.push('\0');  
                    }, 
                        'n' => { literal_chars.push('\n');  
                    },
                    _ => {
                        let mut loc = token.loc;
                        loc.col += i + 2;
                        let loc_msg = fmt_loc_err( self.filename_locations, &token.loc);
                        user_error!("{} unknown escape character  '{}'", loc_msg, ch );
                    }
                }
                i += 2;
            } else {
                literal_chars.push(ch);
                i += 1;
            }
        }

        literal_chars.into_iter().collect()
    }


    fn parse_func_call_args(&mut self) -> Vec<AstExpr> {

        println!("parse_func_call_args");

        self.lexer.expect_token_next(TokenKind::OpenParen);
        // AF TODO only one param, FIX mixup of statements and EXPR
        let firstarg_expr = self.parse_expr();

        self.lexer.expect_token_next(TokenKind::CloseParen);

        vec![firstarg_expr]
        // match firstarg_expr {
        //     // AF TODO
        //     AstExpr::LitString(func_expr) => {
        //         let args = vec!(func_expr);
        //         return args

        //     },
        //     _ => {
        //         todo!()
        //     },
        // }
        // user_error!("Expected function argument EXPR");
    }

    fn parse_func_call(&mut self) -> AstFunCall {

        println!("parse_func_call");

        let token = self.lexer.expect_token_next(TokenKind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);

        let args = self.parse_func_call_args();

        if name =="write" {
            // AF TODO FILL IN ATCUAL EXPRESSION
            return AstFunCall { name, args, loc : token.loc };
        }
        let loc_msg = fmt_loc_err( self.filename_locations, &token.loc);
        user_error!("{} unknown function namme '{}'", loc_msg, &name);
    }


    fn parse_expr(&mut self) -> AstExpr {


        if let Some( token ) = self.lexer.peek() {

            let name = self.lexer.get_string(token.text_start, token.text_len);
            println!("GOT PEEKED TOKEN FOR EXPR {} Name: {}", token_kind_name(token.token_type), name);

            let expr = match token.token_type {

                TokenKind::Name => {
                    let func_call =self.parse_func_call();
                    AstExpr::FuncCall( func_call )
                },
                TokenKind::Literal => {
                    let literal = self.parse_string_literal();
                     AstExpr::LitString(literal)
                },
                // TokenKind::OpenParen => todo!(),
                // TokenKind::CloseParen => todo!(),
                // TokenKind::OpenCurly => todo!(),
                // TokenKind::CloseCurly => todo!(),
                // TokenKind::Semicolon => todo!(),
                _ => {
                    AstExpr::LitString("TODO EMPTY".to_string())
                }
            };
            expr

        } else {
            let loc_msg = fmt_loc_err( 
                self.filename_locations, 
                &self.lexer.get_location());
            user_error!("{} expected expression, reached end of file", loc_msg);
        }
    }

    fn parse_statement(&mut self) -> AstStatement {

        let expr = self.parse_expr();
        self.lexer.expect_token_next(TokenKind::Semicolon);
        
        AstStatement::Expr( expr)
    }

    fn parse_curly_block(&mut self) -> AstBlock {

        let mut stmts = Vec::new();

        // expect open curly
        self.lexer.expect_token_next(TokenKind::OpenCurly);


        while let Some(token) = self.lexer.peek() {
            if token.token_type == TokenKind::CloseCurly {
                break;
            }
            // add expression to block
            let stmt = self.parse_statement();
            stmts.push( stmt );
            self.lexer.expect_token_next(TokenKind::Semicolon);

        }
        self.lexer.expect_token_next(TokenKind::CloseCurly);

        AstBlock {
            statements: stmts,
        }
    }

    fn parse_proc_def(&mut self) -> AstProcDef {

        // check proc token
        self.lexer.expect_keyword("proc");

        // expect name of proc token
        let token = self.lexer.expect_token_next(TokenKind::Name);
        let name= self.lexer.get_string( token.text_start, token.text_len );

        // open and close paren
        self.lexer.expect_token_next(TokenKind::OpenParen);
        self.lexer.expect_token_next(TokenKind::CloseParen);

        let body = self.parse_curly_block();

        let result = AstProcDef{ 
            name,
            body,
        };
        result
    }

    pub fn parse_dump(&mut self) {
        while let Some(nt) = self.lexer.next() {

            // {
            //     let tok_str = tokenizer.get_string( nt.text_start, nt.text_len );
            //     println!("next line row:{} indx: {} :{}:", &nt.loc.row, &nt.loc.col, tok_str );
            // }
            let tok_str = self.lexer.get_string( nt.text_start, nt.text_len );
            let tok_type_name = token_kind_name(nt.token_type);
    
            let err_str = fmt_loc_err(self.filename_locations, &nt.loc);
            println!("TOKEN {} {}  :{}:", &tok_type_name, &err_str, &tok_str );
           // tokenizer.dump();
            //tokenizer.next_line();
        }        

    }

    pub fn parse(&mut self) ->AstProcDef{
        let proc_def = self.parse_proc_def();
        proc_def

        // while let Some(nt) = self.lexer.next() {

        //     let tok_str = self.lexer.get_string( nt.text_start, nt.text_len );
        //     let tok_type_name = token_type_name(nt.token_type);
    
        //     let err_str = fmt_loc_err(self.filename_locations, &nt.loc);
        //     println!("TOKEN {} {}  :{}:", &tok_type_name, &err_str, &tok_str );
        // }        

    }
}