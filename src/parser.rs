use crate::{lexer::Lexer, location::{FileNameLocations, Location, fmt_loc_err}, token::{Token, TokenKind, token_kind_name}};
extern crate static_assertions as sa;

use variant_count::VariantCount;

#[derive(Debug)]
pub struct AstIfStatement {
    pub loc  : Location,    
    pub condition  : AstExpr,
    pub then_block : Box<AstBlock>,
    pub else_block : Option<Box<AstBlock>>, 
}

#[derive(Debug)]
pub struct AstWhileStatement {
    pub loc        : Location,    
    pub condition  : AstExpr,
    pub block      : Box<AstBlock>,
}

#[derive(Debug, VariantCount)]
pub enum AstStatement {
    Expr( AstExpr),
    If( AstIfStatement ),
    VarAssign(AstVarAssign),
    While( AstWhileStatement ),
}

#[derive(Debug)]
pub struct AstFunCall {
    pub loc  : Location,
    pub name : String,
    pub args : Vec<AstExpr>,
}

#[derive(Debug)]
pub struct AstVarRead {
    pub loc  : Location,    
    pub name : String,
} 


#[derive(Debug, VariantCount)]
pub enum AstExpr {
    FuncCall(AstFunCall),
    LitFloat(f64),
    LitInt(i64),
    LitChar(char),
    LitString(String),
    LitBool(bool),
    VarRead(AstVarRead),
}

#[derive(Debug)]
pub struct AstBlock{
    pub statements : Vec<AstStatement>,
} 

#[derive(Debug)]
pub struct AstProcDef {
    pub loc  : Location,
    pub name : String,
    pub body : AstBlock,
} 

#[derive(Debug, VariantCount)]
pub enum AstTypes {
    I64,
}

#[derive(Debug)]
pub struct AstVarDef {
    pub loc      : Location,    
    pub name     : String,
    pub var_type : AstTypes,
} 

#[derive(Debug)]
pub struct AstVarAssign {
    pub loc  : Location,    
    pub name : String,
    pub expr : AstExpr,
}

#[derive(Debug, VariantCount)]
pub enum AstTop {
    ProcDef(AstProcDef),
    VarDef(AstVarDef),
}

#[derive(Debug)]
pub struct AstModule {
    pub tops: Vec<AstTop>,
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

        println!("---------- PARSE STRING LITERAL ");

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

        println!("---------- PARSE FUNC_CALL ARGS ");

        self.lexer.expect_token_next(TokenKind::OpenParen);

        let firstarg_expr = self.parse_expr();

        self.lexer.expect_token_next(TokenKind::CloseParen);

        vec![firstarg_expr]
    }

    fn parse_func_call(&mut self) -> AstFunCall {

        println!("---------- PARSE FUNC_CALL ");

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

    fn parse_var_read(&mut self) -> AstVarRead {
        println!("---------- PARSE VAR READ ");
        let token = self.lexer.expect_token_next(TokenKind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);
        return AstVarRead { name, loc : token.loc };
    }
 

    fn parse_expr(&mut self) -> AstExpr {

        println!("---------- PARSE EXPR ");

        if let Some( token ) = self.lexer.peek(0) {

            let name = self.lexer.get_string(token.text_start, token.text_len);
            println!("GOT PEEKED TOKEN FOR EXPR {} Name: {}", token_kind_name(token.token_kind), name);


            // the code below currently expects 1 type
            // force compiler error when adding new variant 
            //sa::const_assert!(AstExpr::VARIANT_COUNT ==  1);

            let expr = match token.token_kind {

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
                            // check var assignment statement
                            if let Some(next_token) = self.lexer.peek(1) {
                                if next_token.token_kind == TokenKind::OpenParen  {
                                    let func_call =self.parse_func_call();
                                    return AstExpr::FuncCall( func_call )
                                }
                            }

                            let var_read  =self.parse_var_read();
                            AstExpr::VarRead( var_read )
                        },
                    }
                },
                TokenKind::Number => {
                    let _ = self.lexer.next();

                    if let Ok(ivalue) = name.parse::<i64>() {
                        return AstExpr::LitInt(ivalue)
                    };
                    let loc_msg = fmt_loc_err( self.filename_locations, &token.loc);
                    user_error!("{} can't convert number {} to i64", loc_msg, &name);                    

                },
                TokenKind::Literal => {
                    let literal = self.parse_string_literal();
                     AstExpr::LitString(literal)
                },
                TokenKind::Colon        |
                TokenKind::Equals       |
                TokenKind::OpenParen    |
                TokenKind::CloseParen   |
                TokenKind::OpenCurly    |
                TokenKind::CloseCurly   |
                TokenKind::Semicolon => {
                    let loc_msg = fmt_loc_err( self.filename_locations, &token.loc);
                    user_error!("{} expression for token kind {} not implemented", loc_msg, token_kind_name(token.token_kind));                    
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

        println!("---------- PARSE IF STATMENT ");

        let token = self.lexer.expect_keyword("if");

        // open and close paren
        self.lexer.expect_token_next(TokenKind::OpenParen);
        let expr = self.parse_expr();
        self.lexer.expect_token_next(TokenKind::CloseParen);
        let then_block = Box::new( self.parse_curly_block() ); 

        let mut else_block = None;

        if let Some( token ) = self.lexer.peek(0) {
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

    fn parse_while(&mut self) ->AstStatement {

        println!("---------- PARSE WHILE STATMENT ");

        let token = self.lexer.expect_keyword("while");

        // open and close paren
        self.lexer.expect_token_next(TokenKind::OpenParen);
        let expr = self.parse_expr();
        self.lexer.expect_token_next(TokenKind::CloseParen);
        let block = Box::new( self.parse_curly_block() ); 


        AstStatement::While( AstWhileStatement {
            loc : token.loc,
            condition: expr,
            block,
        })
    }

    fn parse_statement(&mut self) -> AstStatement {

        println!("---------- PARSE STATMENT ");

        if let Some( token ) = self.lexer.peek(0) {

            let stmt = match token.token_kind {

                TokenKind::Name => {

                    // check if statement
                    if self.lexer.is_keyword(&token, "if") {
                        return self.parse_if();
                    } 

                    // check var assignment statement
                    if let Some(next_token) = self.lexer.peek(1) {
                        if next_token.token_kind == TokenKind::Equals  {
                            return self.parse_var_assign()
                        }
                    }

                    // check if statement
                    if self.lexer.is_keyword(&token, "while") {
                        return self.parse_while();
                    } 

                    println!("---------- PARSE STATMENT AS EXPRESSION ");
                    AstStatement::Expr( self.parse_expr() )

                },
                TokenKind::Number |
                TokenKind::Literal |
                TokenKind::OpenParen |
                TokenKind::CloseParen |
                TokenKind::OpenCurly |
                TokenKind::CloseCurly |
                TokenKind::Colon |
                TokenKind::Equals |
                TokenKind::Semicolon  => {

                let loc_msg = fmt_loc_err( 
                    self.filename_locations, 
                    &token.loc);
                user_error!("{} statement starts with {} ", loc_msg, token_kind_name(token.token_kind));                }
            };
            
            // default to an expression
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


        while let Some(token) = self.lexer.peek(0) {
            if token.token_kind == TokenKind::CloseCurly {
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
        println!("---------- PARSE PROC DEF ");

        // check proc token
        let token = self.lexer.expect_keyword("proc");
        let loc = token.loc;

        // expect name of proc token
        let token = self.lexer.expect_token_next(TokenKind::Name);
        let name= self.lexer.get_string( token.text_start, token.text_len );

        // open and close paren
        self.lexer.expect_token_next(TokenKind::OpenParen);

        self.lexer.expect_token_next(TokenKind::CloseParen);

        let body = self.parse_curly_block();

        let result = AstProcDef{ 
            loc,
            name,
            body,
        };
        result
    }

    fn parse_type(&mut self) -> AstTypes {

        println!("---------- PARSE TYPE ");
        // expect type name
        let token = self.lexer.expect_token_next(TokenKind::Name);
        let type_name = self.lexer.get_string(token.text_start, token.text_len);
        println!("VAR TYPE NAME IS: {} ", type_name);
        
        match type_name.as_str() {

            "i64" => {
                AstTypes::I64
            },
            type_name => {
                // the code below currently expects 1 type
                // force compiler error when adding new variant 
                sa::const_assert!(AstTypes::VARIANT_COUNT ==  1);
            
                let loc_msg = fmt_loc_err( 
                    self.filename_locations, 
                    &self.lexer.get_location());

                user_error!("{} unknown file type {}", loc_msg, &type_name);
            }
        }
    }


    fn parse_var_def(&mut self) -> AstVarDef {
        println!("---------- PARSE VAR DEF");

        // check var token
        let token = self.lexer.expect_keyword("var");
        let loc = token.loc;

        // expect name of var token
        let token = self.lexer.expect_token_next(TokenKind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);
        
        // expect colon
        self.lexer.expect_token_next(TokenKind::Colon);

        let var_type = self.parse_type();

        // expect semicolon
        let _ = self.lexer.expect_token_next(TokenKind::Semicolon);

        AstVarDef { loc, name, var_type }
    }


    fn parse_var_assign(&mut self) -> AstStatement {
        println!("---------- PARSE VAR ASSIGN ");

        // check var token
        let token = self.lexer.expect_token_next(TokenKind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);
        let loc = token.loc;
        let _ = self.lexer.expect_token_next(TokenKind::Equals);
        let expr  = self.parse_expr();
        let _ = self.lexer.expect_token_next(TokenKind::Semicolon);

        AstStatement::VarAssign( AstVarAssign {
            loc,
            name,
            expr,
        } )
    }


    fn parse_top(&mut self, token : &Token) -> AstTop {
        println!("---------- PARSE TOP ");

        if self.lexer.is_keyword(&token, "proc") {
            return AstTop::ProcDef( self.parse_proc_def() );
        }

        if self.lexer.is_keyword(&token, "var") {
            return AstTop::VarDef( self.parse_var_def() );
        }          

        let loc_msg = fmt_loc_err( 
            self.filename_locations, 
            &token.loc);

        // the code below currently expects 2 top level variant
        // force compiler error when adding new variant 
        sa::const_assert!(AstTop::VARIANT_COUNT ==  2);
                user_error!("{} expected var or proc, got {} ", 
            loc_msg, 
            token_kind_name(token.token_kind));
    }

    fn parse_module(&mut self) -> AstModule {
        let mut tops = Vec::new();

        // while we got tokens left, parse top defs
        while let Some(token) = self.lexer.peek(0) {
            tops.push( self.parse_top(&token) );
        }

        AstModule {
            tops,
        }
    }

    pub fn parse(&mut self) ->AstModule {
        self.parse_module()
    }
}