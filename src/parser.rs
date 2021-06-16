use crate::{lexer::Lexer, location::{FileNameLocations, Location, fmt_loc_err}, token::{Token, TokenKind, token_kind_name}};
extern crate static_assertions as sa;

use variant_count::VariantCount;

#[derive(Debug, Clone)]
pub struct AstIfStatement {
    pub loc  : Location,    
    pub condition  : AstExpr,
    pub then_block : Box<AstBlock>,
    pub else_block : Option<Box<AstBlock>>, 
}

#[derive(Debug, Clone)]
pub struct AstWhileStatement {
    pub loc        : Location,    
    pub condition  : AstExpr,
    pub block      : Box<AstBlock>,
}

#[derive(Debug, Clone, VariantCount)]
pub enum AstStatement {
    Expr( AstExpr),
    If( AstIfStatement ),
    VarAssign(AstVarAssign),
    While( AstWhileStatement ),
}

#[derive(Debug, Clone)]
pub struct AstFunCall {
    pub loc  : Location,
    pub name : String,
    pub args : Vec<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstVarRead {
    pub loc  : Location,    
    pub name : String,
} 

#[derive(Debug, Clone, VariantCount)]
pub enum AstBinaryOpKind {
    Plus,
    Less,
}

#[derive(Debug, Clone)]
pub struct AstBinaryOp {
    pub loc  : Location,
    pub kind : AstBinaryOpKind,
    pub lhs  : Box<AstExpr>,
    pub rhs  : Box<AstExpr>,
} 

#[derive(Debug, Clone, VariantCount)]
pub enum AstExpr {
    FuncCall(AstFunCall),
    LitFloat(f64),
    LitInt(i64),
    LitChar(char),
    LitString(String),
    LitBool(bool),
    VarRead(AstVarRead),
    BinarayOp(AstBinaryOp),
}

#[derive(Debug, Clone)]
pub struct AstBlock{
    pub statements : Vec<AstStatement>,
} 

#[derive(Debug, Clone)]
pub struct AstProcDef {
    pub loc  : Location,
    pub name : String,
    pub body : AstBlock,
} 

#[derive(Debug, Clone, Copy,  PartialEq, VariantCount)]
pub enum AstTypes {
    VOID,
    I64,
    BOOL,
    PTR,
}

#[derive(Debug, Clone)]
pub struct AstVarDef {
    pub loc      : Location,    
    pub name     : String,
    pub var_type : AstTypes,
} 

#[derive(Debug, Clone)]
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


pub fn expr_kind_to_name(expr : &AstExpr) -> &'static str {
    match expr {
        AstExpr::FuncCall(_) => "function call",
        AstExpr::LitFloat(_) => "float literal",
        AstExpr::LitInt(_) => "integral literal",
        AstExpr::LitChar(_) => "char literal",
        AstExpr::LitString(_) => "string literal",
        AstExpr::LitBool(_) => "bool literal",
        AstExpr::VarRead(_) => "read variable",
        AstExpr::BinarayOp(_) => "binary operator",
    }
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
        let mut args_expr = Vec::new();

        self.lexer.expect_token_next(TokenKind::OpenParen);


        if let Some( next_token ) = self.lexer.peek(0) {
            if  next_token.token_kind != TokenKind::CloseParen {
                let firstarg_expr = self.parse_expr();
                args_expr.push(firstarg_expr);
            }
        }
        self.lexer.expect_token_next(TokenKind::CloseParen);

        args_expr
    }

    fn parse_func_call(&mut self) -> AstFunCall {

        println!("---------- PARSE FUNC_CALL ");

        let token = self.lexer.expect_token_next(TokenKind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);

        let args = self.parse_func_call_args();
        AstFunCall { loc : token.loc, name, args }
    }

    fn parse_var_read(&mut self) -> AstVarRead {
        println!("---------- PARSE VAR READ ");
        let token = self.lexer.expect_token_next(TokenKind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);
        AstVarRead { loc : token.loc, name }
    }
 
    fn parse_primary_expr(&mut self) -> AstExpr {

        println!("---------- PARSE PRIMARY EXPR ");

        if let Some( token ) = self.lexer.peek(0) {

            let name = self.lexer.get_string(token.text_start, token.text_len);
            println!("PRIMARY EXPR PEEKED TOKEN FOR EXPR {} Name: {}", token_kind_name(token.token_kind), name);

            // the code below currently expects 1 type
            // force compiler error when adding new variant 
            //sa::const_assert!(AstExpr::VARIANT_COUNT ==  1);
            let expr = match token.token_kind {

                TokenKind::Name => {
                    match name.as_str() {
                        "true" => {
                            self.lexer.extract_next();
                            AstExpr::LitBool(true)
                        },
                        "false" => {
                            self.lexer.extract_next();
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
                    let _ = self.lexer.extract_next();

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
                TokenKind::Plus         |
                TokenKind::Less         |
                TokenKind::Semicolon => {
                    let loc_msg = fmt_loc_err( self.filename_locations, &token.loc);
                    user_error!("{} primary expression for token kind {} doesn't exist", loc_msg, token_kind_name(token.token_kind));                    
                }
            };
            expr

        } else {
            let loc_msg = fmt_loc_err( 
                self.filename_locations, 
                &self.lexer.get_location());
            user_error!("{} expected primary expression, reached end of file", loc_msg);
        }
    }

    fn parse_expr(&mut self) -> AstExpr {

        println!("---------- PARSE EXPR ");
        let lhs = self.parse_primary_expr();

        if let Some( token ) = self.lexer.peek(0) {

            let name = self.lexer.get_string(token.text_start, token.text_len);
            println!("EXPR: PEEKED TOKEN FOR EXPR {} Name: {}", token_kind_name(token.token_kind), name);

            match &token.token_kind {
                // groep binary operators?
                TokenKind::Plus => {
                    self.lexer.extract_next();
                    let rhs = self.parse_expr();
                    return AstExpr::BinarayOp( AstBinaryOp {
                        loc : token.loc,
                        kind: AstBinaryOpKind::Plus,
                        lhs : Box::new(lhs),
                        rhs : Box::new(rhs),
                    })
                },
                TokenKind::Less=> {
                    self.lexer.extract_next();
                    let rhs = self.parse_expr();
                    return AstExpr::BinarayOp( AstBinaryOp {
                        loc : token.loc,
                        kind: AstBinaryOpKind::Less,
                        lhs : Box::new(lhs),
                        rhs : Box::new(rhs),
                    })
                },
                TokenKind::Name       |
                TokenKind::Number     |
                TokenKind::OpenParen  |
                TokenKind::CloseParen |
                TokenKind::OpenCurly  |
                TokenKind::CloseCurly |
                TokenKind::Semicolon  |
                TokenKind::Literal    |
                TokenKind::Colon      |
                TokenKind::Equals => {
                    // fallthrough
                },
            }
        }
        lhs
    }

    fn parse_if(&mut self) ->AstStatement {

        println!("---------- PARSE IF STATMENT ");

        let token = self.lexer.expect_keyword("if");

        // open and close paren
        let expr = self.parse_expr();
        let then_block = Box::new( self.parse_curly_block() ); 

        let mut else_block = None;

        if let Some( token ) = self.lexer.peek(0) {
            if self.lexer.is_keyword(&token, "else") {
                self.lexer.extract_next();
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
        let expr = self.parse_expr();
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
            match token.token_kind {
                TokenKind::Name => {

                    // check if statement, return no semicolon needed
                    if self.lexer.is_keyword(&token, "if") {
                        return self.parse_if();
                    } 

                    // check if statement, return no semicolon needed
                    if self.lexer.is_keyword(&token, "while") {
                        return self.parse_while();
                    }

                    // check var assignment statement, semicoln parsed inside var_assign
                    if let Some(next_token) = self.lexer.peek(1) {
                        if next_token.token_kind == TokenKind::Equals  {
                            return self.parse_var_assign()
                        }
                    }
                },
                TokenKind::Number |
                TokenKind::Literal |
                TokenKind::OpenParen |
                TokenKind::CloseParen |
                TokenKind::OpenCurly |
                TokenKind::CloseCurly |
                TokenKind::Colon |
                TokenKind::Equals |
                TokenKind::Plus |
                TokenKind::Less |
                TokenKind::Semicolon  => {
                    // fallthrough, parse as an expression with a semicolon
                }
            };
            
            // parse as exprssion with a semicolon
            println!("---------- PARSE STATMENT AS EXPRESSION ");
            let stmt = AstStatement::Expr( self.parse_expr() );
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

        AstProcDef{ 
            loc,
            name,
            body,
        }
    }

    fn parse_type(&mut self) -> AstTypes {

        println!("---------- PARSE TYPE ");
        // expect type name
        let token = self.lexer.expect_token_next(TokenKind::Name);
        let type_name = self.lexer.get_string(token.text_start, token.text_len);
        println!("VAR TYPE NAME IS: {} ", type_name);
        
        match type_name.as_str() {

            "void" => {
                AstTypes::VOID
            },
            "i64" => {
                AstTypes::I64
            },
            "bool" => {
                AstTypes::BOOL
            },
            "ptr" => {
                AstTypes::PTR
            },
            type_name => {
                // the code below currently expects 1 type
                // force compiler error when adding new variant 
                sa::const_assert!(AstTypes::VARIANT_COUNT ==  4);
            
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