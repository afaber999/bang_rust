use crate::{
    lexer::Lexer,
    location::{fmt_loc_err, FileNameLocations, Location},
    token::{Token, Kind},
};
extern crate static_assertions as sa;

use variant_count::VariantCount;

#[derive(Debug, Clone)]
pub struct AstIfStatement {
    pub loc: Location,
    pub condition: AstExpr,
    pub then_block: Box<AstBlock>,
    pub else_block: Option<Box<AstBlock>>,
}

#[derive(Debug, Clone)]
pub struct AstWhileStatement {
    pub loc: Location,
    pub condition: AstExpr,
    pub block: Box<AstBlock>,
}

#[derive(Debug, Clone, VariantCount)]
pub enum AstStatement {
    Expr(AstExpr),
    If(AstIfStatement),
    VarAssign(AstVarAssign),
    While(AstWhileStatement),
}

#[derive(Debug, Clone)]
pub struct AstFunCall {
    pub loc: Location,
    pub name: String,
    pub args: Vec<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstVarRead {
    pub loc: Location,
    pub name: String,
}

#[derive(Debug, Clone, Copy, VariantCount)]
pub enum AstBinaryOpKind {
    Plus,
    Less,
    Mult,
}

#[derive(Debug, Clone)]
pub struct AstBinaryOp {
    pub loc: Location,
    pub kind: AstBinaryOpKind,
    pub lhs: Box<AstExpr>,
    pub rhs: Box<AstExpr>,
}

#[derive(Debug, Clone, VariantCount)]
pub enum AstExprKind {
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
pub struct AstExpr {
    pub loc: Location,
    pub kind: AstExprKind,
}

#[derive(Debug, Clone)]
pub struct AstBlock {
    pub statements: Vec<AstStatement>,
}

#[derive(Debug, Clone)]
pub struct AstProcDef {
    pub loc: Location,
    pub name: String,
    pub body: AstBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, VariantCount)]
pub enum AstTypes {
    VOID,
    I64,
    BOOL,
    PTR,
}

#[derive(Debug, Clone)]
pub struct AstVarDef {
    pub loc: Location,
    pub name: String,
    pub var_type: AstTypes,
}

#[derive(Debug, Clone)]
pub struct AstVarAssign {
    pub loc: Location,
    pub name: String,
    pub expr: AstExpr,
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

pub fn expr_kind_to_name(expr: &AstExpr) -> &'static str {
    match expr.kind {
        AstExprKind::FuncCall(_) => "function call",
        AstExprKind::LitFloat(_) => "float literal",
        AstExprKind::LitInt(_) => "integral literal",
        AstExprKind::LitChar(_) => "char literal",
        AstExprKind::LitString(_) => "string literal",
        AstExprKind::LitBool(_) => "bool literal",
        AstExprKind::VarRead(_) => "read variable",
        AstExprKind::BinarayOp(_) => "binary operator",
    }
}

pub fn name_to_type(type_name: &str) -> Option<AstTypes> {
    match type_name {
        "void" => Some(AstTypes::VOID),
        "i64" => Some(AstTypes::I64),
        "bool" => Some(AstTypes::BOOL),
        "ptr" => Some(AstTypes::PTR),
        _ => None,
    }
}

pub fn type_to_str(type_kind: AstTypes) -> &'static str {
    match type_kind {
        AstTypes::VOID => "void",
        AstTypes::I64 => "i64",
        AstTypes::BOOL => "bool",
        AstTypes::PTR => "ptr",
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    filename_locations: &'a FileNameLocations,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, filename_locations: &'a FileNameLocations) -> Self {
        Self {
            lexer,
            filename_locations,
        }
    }

    fn parse_string_literal(&mut self) -> String {
        println!("---------- PARSE STRING LITERAL ");

        let token = self.lexer.expect_token_next(Kind::Literal);

        let mut i = token.text_start + 1;
        let last_index = i + token.text_len - 2;

        let mut literal_chars = Vec::new();

        while i < last_index {
            let ch = self.lexer.get_char(i);
            if ch == '\\' {
                if i + 1 >= last_index {
                    let mut loc = token.loc;
                    loc.col += i + 1;
                    let loc_msg = fmt_loc_err(self.filename_locations, &token.loc);
                    user_error!("{} unfinished string literal escape sequence", loc_msg);
                }

                let ch = self.lexer.get_char(i + 1);
                match ch {
                    '0' => {
                        literal_chars.push('\0');
                    }
                    'n' => {
                        literal_chars.push('\n');
                    }
                    _ => {
                        let mut loc = token.loc;
                        loc.col += i + 2;
                        let loc_msg = fmt_loc_err(self.filename_locations, &token.loc);
                        user_error!("{} unknown escape character  '{}'", loc_msg, ch);
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

        self.lexer.expect_token_next(Kind::OpenParen);

        if let Some(next_token) = self.lexer.peek(0) {
            if next_token.token_kind != Kind::CloseParen {
                args_expr.push(self.parse_expr());
            }
        }

        while let Some(next_token) = self.lexer.peek(0) {
            if next_token.token_kind != Kind::Comma {
                break;
            }
            // consume comma token
            self.lexer.expect_token_next(Kind::Comma);
            args_expr.push(self.parse_expr());
        }

        // expect close paren
        self.lexer.expect_token_next(Kind::CloseParen);

        args_expr
    }

    fn parse_func_call(&mut self) -> AstFunCall {
        println!("---------- PARSE FUNC_CALL ");

        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);

        let args = self.parse_func_call_args();
        AstFunCall {
            loc: token.loc,
            name,
            args,
        }
    }

    fn parse_var_read(&mut self) -> AstVarRead {
        println!("---------- PARSE VAR READ ");
        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);
        AstVarRead {
            loc: token.loc,
            name,
        }
    }

    fn parse_primary_expr(&mut self) -> AstExpr {
        println!("---------- PARSE PRIMARY EXPR ");

        if let Some(token) = self.lexer.peek(0) {
            let name = self.lexer.get_string(token.text_start, token.text_len);
            println!(
                "PRIMARY EXPR PEEKED TOKEN FOR EXPR {} Name: {}",
                Token::kind_name(token.token_kind),
                name
            );

            let loc = token.loc;

            match token.token_kind {
                Kind::Name => {
                    return match name.as_str() {
                        "true" => {
                            self.lexer.extract_next();

                            AstExpr {
                                loc,
                                kind: AstExprKind::LitBool(true),
                            }
                        }
                        "false" => {
                            self.lexer.extract_next();
                            AstExpr {
                                loc,
                                kind: AstExprKind::LitBool(false),
                            }
                        }
                        _ => {
                            // check var assignment statement
                            if let Some(next_token) = self.lexer.peek(1) {
                                if next_token.token_kind == Kind::OpenParen {
                                    let func_call = self.parse_func_call();

                                    return AstExpr {
                                        loc,
                                        kind: AstExprKind::FuncCall(func_call),
                                    };
                                }
                            }

                            let var_read = self.parse_var_read();

                            AstExpr {
                                loc,
                                kind: AstExprKind::VarRead(var_read),
                            }
                        }
                    };
                }
                Kind::Number => {
                    let _ = self.lexer.extract_next();

                    if let Ok(ivalue) = name.parse::<i64>() {
                        return AstExpr {
                            loc,
                            kind: AstExprKind::LitInt(ivalue),
                        };
                    };

                    let loc_msg = fmt_loc_err(self.filename_locations, &token.loc);
                    user_error!("{} can't convert number {} to i64", loc_msg, &name);
                }
                Kind::Literal => {
                    let literal = self.parse_string_literal();
                    return AstExpr {
                        loc,
                        kind: AstExprKind::LitString(literal),
                    };
                }
                Kind::Comma
                | Kind::Colon
                | Kind::Equals
                | Kind::OpenParen
                | Kind::CloseParen
                | Kind::OpenCurly
                | Kind::CloseCurly
                | Kind::Plus
                | Kind::Mult
                | Kind::Less
                | Kind::Semicolon => {
                    let loc_msg = fmt_loc_err(self.filename_locations, &token.loc);
                    user_error!(
                        "{} primary expression for token kind {} doesn't exist",
                        loc_msg,
                        Token::kind_name(token.token_kind)
                    );
                }
            };
        } else {
            let loc_msg = fmt_loc_err(self.filename_locations, &self.lexer.get_location());
            user_error!(
                "{} expected primary expression, reached end of file",
                loc_msg
            );
        }
    }

    fn parse_expr(&mut self) -> AstExpr {
        println!("---------- PARSE EXPR ");
        let lhs = self.parse_primary_expr();

        if let Some(token) = self.lexer.peek(0) {
            let name = self.lexer.get_string(token.text_start, token.text_len);
            println!(
                "EXPR: PEEKED TOKEN FOR EXPR {} Name: {}",
                Token::kind_name(token.token_kind),
                name
            );

            match &token.token_kind {
                // groep binary operators?
                Kind::Plus => {
                    self.lexer.extract_next();
                    let rhs = self.parse_expr();

                    let kind = AstExprKind::BinarayOp(AstBinaryOp {
                        loc: token.loc,
                        kind: AstBinaryOpKind::Plus,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });

                    return AstExpr {
                        loc: token.loc,
                        kind,
                    };
                }
                Kind::Mult => {
                    self.lexer.extract_next();
                    let rhs = self.parse_expr();

                    let kind = AstExprKind::BinarayOp(AstBinaryOp {
                        loc: token.loc,
                        kind: AstBinaryOpKind::Mult,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });

                    return AstExpr {
                        loc: token.loc,
                        kind,
                    };
                }

                Kind::Less => {
                    self.lexer.extract_next();
                    let rhs = self.parse_expr();
                    let kind = AstExprKind::BinarayOp(AstBinaryOp {
                        loc: token.loc,
                        kind: AstBinaryOpKind::Less,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });
                    return AstExpr {
                        loc: token.loc,
                        kind,
                    };
                }
                Kind::Name
                | Kind::Number
                | Kind::OpenParen
                | Kind::CloseParen
                | Kind::OpenCurly
                | Kind::CloseCurly
                | Kind::Semicolon
                | Kind::Literal
                | Kind::Colon
                | Kind::Comma
                | Kind::Equals => {
                    // fallthrough
                }
            }
        }
        lhs
    }

    fn parse_if(&mut self) -> AstStatement {
        println!("---------- PARSE IF STATMENT ");

        let token = self.lexer.expect_keyword("if");

        // open and close paren
        let expr = self.parse_expr();
        let then_block = Box::new(self.parse_curly_block());

        let mut else_block = None;

        if let Some(token) = self.lexer.peek(0) {
            if self.lexer.is_keyword(&token, "else") {
                self.lexer.extract_next();
                println!("ELSE block");
                else_block = Some(Box::new(self.parse_curly_block()));
                println!("END ELSE block");
            }
        }

        AstStatement::If(AstIfStatement {
            loc: token.loc,
            condition: expr,
            then_block,
            else_block,
        })
    }

    fn parse_while(&mut self) -> AstStatement {
        println!("---------- PARSE WHILE STATMENT ");

        let token = self.lexer.expect_keyword("while");

        // open and close paren
        let expr = self.parse_expr();
        let block = Box::new(self.parse_curly_block());

        AstStatement::While(AstWhileStatement {
            loc: token.loc,
            condition: expr,
            block,
        })
    }

    fn parse_statement(&mut self) -> AstStatement {
        println!("---------- PARSE STATMENT ");

        if let Some(token) = self.lexer.peek(0) {
            match token.token_kind {
                Kind::Name => {
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
                        if next_token.token_kind == Kind::Equals {
                            return self.parse_var_assign();
                        }
                    }
                }
                Kind::Number
                | Kind::Literal
                | Kind::OpenParen
                | Kind::CloseParen
                | Kind::OpenCurly
                | Kind::CloseCurly
                | Kind::Colon
                | Kind::Equals
                | Kind::Plus
                | Kind::Mult
                | Kind::Less
                | Kind::Comma
                | Kind::Semicolon => {
                    // fallthrough, parse as an expression with a semicolon
                }
            };

            // parse as exprssion with a semicolon
            println!("---------- PARSE STATMENT AS EXPRESSION ");
            let stmt = AstStatement::Expr(self.parse_expr());
            self.lexer.expect_token_next(Kind::Semicolon);
            stmt
        } else {
            let loc_msg = fmt_loc_err(self.filename_locations, &self.lexer.get_location());
            user_error!("{} expected statement, reached end of file", loc_msg);
        }
    }

    fn parse_curly_block(&mut self) -> AstBlock {
        println!("---------- CURLY BLOCK ");
        let mut stmts = Vec::new();

        // expect open curly
        self.lexer.expect_token_next(Kind::OpenCurly);

        while let Some(token) = self.lexer.peek(0) {
            if token.token_kind == Kind::CloseCurly {
                break;
            }
            // add expression to block
            let stmt = self.parse_statement();
            stmts.push(stmt);
        }
        self.lexer.expect_token_next(Kind::CloseCurly);

        AstBlock { statements: stmts }
    }

    fn parse_proc_def(&mut self) -> AstProcDef {
        println!("---------- PARSE PROC DEF ");

        // check proc token
        let token = self.lexer.expect_keyword("proc");
        let loc = token.loc;

        // expect name of proc token
        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);

        // open and close paren
        self.lexer.expect_token_next(Kind::OpenParen);

        self.lexer.expect_token_next(Kind::CloseParen);

        let body = self.parse_curly_block();

        AstProcDef { loc, name, body }
    }

    fn parse_type(&mut self) -> AstTypes {
        println!("---------- PARSE TYPE ");
        // expect type name
        let token = self.lexer.expect_token_next(Kind::Name);
        let type_name = self.lexer.get_string(token.text_start, token.text_len);
        println!("VAR TYPE NAME IS: {} ", type_name);

        if let Some(type_kind) = name_to_type(type_name.as_str()) {
            return type_kind;
        }

        let loc_msg = fmt_loc_err(self.filename_locations, &self.lexer.get_location());

        user_error!("{} unknown file type {}", loc_msg, &type_name);
    }

    fn parse_var_def(&mut self) -> AstVarDef {
        println!("---------- PARSE VAR DEF");

        // check var token
        let token = self.lexer.expect_keyword("var");
        let loc = token.loc;

        // expect name of var token
        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);

        // expect colon
        self.lexer.expect_token_next(Kind::Colon);

        let var_type = self.parse_type();

        // expect semicolon
        let _ = self.lexer.expect_token_next(Kind::Semicolon);

        AstVarDef {
            loc,
            name,
            var_type,
        }
    }

    fn parse_var_assign(&mut self) -> AstStatement {
        println!("---------- PARSE VAR ASSIGN ");

        // check var token
        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);
        let loc = token.loc;
        let _ = self.lexer.expect_token_next(Kind::Equals);
        let expr = self.parse_expr();
        let _ = self.lexer.expect_token_next(Kind::Semicolon);

        AstStatement::VarAssign(AstVarAssign { loc, name, expr })
    }

    fn parse_top(&mut self, token: &Token) -> AstTop {
        println!("---------- PARSE TOP ");

        if self.lexer.is_keyword(&token, "proc") {
            return AstTop::ProcDef(self.parse_proc_def());
        }

        if self.lexer.is_keyword(&token, "var") {
            return AstTop::VarDef(self.parse_var_def());
        }

        let loc_msg = fmt_loc_err(self.filename_locations, &token.loc);

        // the code below currently expects 2 top level variant
        // force compiler error when adding new variant
        sa::const_assert!(AstTop::VARIANT_COUNT == 2);
        user_error!(
            "{} expected var or proc, got {} ",
            loc_msg,
            Token::kind_name(token.token_kind)
        );
    }

    fn parse_module(&mut self) -> AstModule {
        let mut tops = Vec::new();

        // while we got tokens left, parse top defs
        while let Some(token) = self.lexer.peek(0) {
            tops.push(self.parse_top(&token));
        }

        AstModule { tops }
    }

    pub fn parse(&mut self) -> AstModule {
        self.parse_module()
    }
}
