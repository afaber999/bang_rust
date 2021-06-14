use crate::{lexer::Lexer, location::{FileNameLocations, Location, fmt_loc_err}, token::{TokenKind, token_kind_name}};


#[derive(Debug)]
pub struct AstIfStatement {
    pub loc  : Location,    
    pub condition  : AstExpr,
    pub then_block : Box<AstBlock>,
    pub else_block : Option<Box<AstBlock>>, 
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
    LitBool(bool),
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
pub enum AstTypes {
    I64,
}

#[derive(Debug)]
pub struct AstVarDef {
    pub name     : String,
    pub var_type : AstTypes,
} 

#[derive(Debug)]
pub enum AstTop {
    ProcDef(AstProcDef),
    VarDef(AstVarDef),
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

        let firstarg_expr = self.parse_expr();

        self.lexer.expect_token_next(TokenKind::CloseParen);

        vec![firstarg_expr]
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
        user_error!("{} unknown function name '{}'", loc_msg, &name);
    }


    fn parse_expr(&mut self) -> AstExpr {

        println!("---------- PARSE EXPR ");

        if let Some( token ) = self.lexer.peek() {

            let name = self.lexer.get_string(token.text_start, token.text_len);
            println!("GOT PEEKED TOKEN FOR EXPR {} Name: {}", token_kind_name(token.token_type), name);

            let expr = match token.token_type {

                TokenKind::Name => {
                    match name.as_str() {
                        "true" => {
                            self.lexer.next();
                            AstExpr::LitBool(true)
                        },
                        "false" => {
                            self.lexer.next();
                            AstExpr::LitBool(false)
                        },
                        _ =>  {
                            let func_call =self.parse_func_call();
                            AstExpr::FuncCall( func_call )
                        },
                    }
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

    fn parse_if(&mut self) ->AstStatement {

        let token = self.lexer.expect_keyword("if");

        // open and close paren
        println!("IF expect open paren");
        self.lexer.expect_token_next(TokenKind::OpenParen);
        println!("IF parse expr");
        let expr = self.parse_expr();
        println!("IF expect closeB paren");
        self.lexer.expect_token_next(TokenKind::CloseParen);
        println!("IF block");
        let then_block = Box::new( self.parse_curly_block() ); 
        println!("END IF block");

        let mut else_block = None;

        if let Some( token ) = self.lexer.peek() {
            if self.lexer.is_keyword(&token, "else") {
                self.lexer.next();
                println!("ELSE block");
                else_block = Some( Box::new( self.parse_curly_block())); 
                println!("END ELSE block");
            } 
        }

        AstStatement::If( AstIfStatement {
            loc : token.loc,
            condition: expr,
            then_block,
            else_block,    
        })
    }

    fn parse_statement(&mut self) -> AstStatement {

        println!("---------- PARSE STATMENT ");

        if let Some( token ) = self.lexer.peek() {

            let stmt = match token.token_type {

                TokenKind::Name => {
                    if self.lexer.is_keyword(&token, "if") {
                        return self.parse_if();
                    } 
                    println!("---------- PARSE STATMENT AS EXPRESSION ");
                    AstStatement::Expr( self.parse_expr() )

                },
                TokenKind::Literal |
                TokenKind::OpenParen |
                TokenKind::CloseParen |
                TokenKind::OpenCurly |
                TokenKind::CloseCurly |
                TokenKind::Semicolon  => {
                    user_error!("Unknown statement ")
                }
            };
            
            self.lexer.expect_token_next(TokenKind::Semicolon);

            stmt

        } else {
            let loc_msg = fmt_loc_err( 
                self.filename_locations, 
                &self.lexer.get_location());
            user_error!("{} expected statement, reached end of file", loc_msg);
        }
    }

    fn parse_curly_block(&mut self) -> AstBlock {

        println!("---------- CURLY BLOCK ");
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

        println!("!!!!!!!!!! EXPECT CLOSE PAREN FOR PROC");
        self.lexer.expect_token_next(TokenKind::CloseParen);
        println!("!!!!!!!!!! DONE EXPECT CLOSE PAREN FOR PROC");

        let body = self.parse_curly_block();

        let result = AstProcDef{ 
            name,
            body,
        };
        result
    }

    fn parse_var_def(&mut self) -> AstVarDef {
        println!("---------- PARSE VAR ");
        // check proc token
        self.lexer.expect_keyword("var");
        AstVarDef { name: "TODO".to_string(), var_type: AstTypes::I64 }
    }    

    fn parse_top(&mut self) -> AstTop {
        println!("---------- PARSE TOP ");

        if let Some( token ) = self.lexer.peek() {

            if self.lexer.is_keyword(&token, "proc") {
                return AstTop::ProcDef( self.parse_proc_def() );
            }
            if self.lexer.is_keyword(&token, "var") {
                return AstTop::VarDef( self.parse_var_def() );
            }          

            let loc_msg = fmt_loc_err( 
                self.filename_locations, 
                &token.loc);

            user_error!("{} expected var or proc, got {} ", 
                loc_msg, 
                token_kind_name(token.token_type));
        }

        let loc_msg = fmt_loc_err( 
            self.filename_locations, 
            &self.lexer.get_location());
        user_error!("{} expected var or proc definition, reached end of file", loc_msg);
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

    pub fn parse(&mut self) ->AstTop{
        self.parse_top()
    }
}