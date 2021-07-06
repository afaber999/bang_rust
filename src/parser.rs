
use crate::{ast::{AstBinaryOp, AstBinaryOpKind, AstBlock, AstExpr, AstExprKind, AstFunCall, AstIfStatement, AstModule, AstProcDef, AstProcParam, AstStatement, AstTop, AstTypes, AstVarAssign, AstVarDef, AstVarRead, AstWhileStatement, BinaryOpDef, name_to_type}, lexer::Lexer, location::{fmt_loc_err}, token::{Token, Kind}};


extern crate static_assertions as sa;
use crate::precedence::Precedence;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
        }
    }

    fn parse_string_literal(&mut self) -> String {
        // println!("---------- PARSE STRING LITERAL ");

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
                    user_error!("{} unfinished string literal escape sequence",&loc.fmt_err());
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
                        user_error!("{} unknown escape character  '{}'", &loc.fmt_err(), ch);
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

    fn parse_func_call_args(&mut self) -> Vec<AstExpr<'a>> {
        // println!("---------- PARSE FUNC_CALL ARGS ");
        let mut args_expr = Vec::new();

        self.lexer.expect_token_next(Kind::OpenParen);

        if let Some(next_token) = self.lexer.peek(0) {
            if next_token.kind != Kind::CloseParen {
                args_expr.push(self.parse_expr(Precedence::P0));
            }
        }

        while let Some(next_token) = self.lexer.peek(0) {
            if next_token.kind != Kind::Comma {
                break;
            }
            // consume comma token
            self.lexer.expect_token_next(Kind::Comma);
            args_expr.push(self.parse_expr(Precedence::P0));
        }

        // expect close paren
        self.lexer.expect_token_next(Kind::CloseParen);

        args_expr
    }

    fn parse_func_call(&mut self) -> AstFunCall<'a> {
        // println!("---------- PARSE FUNC_CALL ");

        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);

        let args = self.parse_func_call_args();
        AstFunCall {
            loc: token.loc,
            name,
            args,
        }
    }

    fn parse_var_read(&mut self) -> AstVarRead<'a> {
        // println!("---------- PARSE VAR READ ");
        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);
        AstVarRead {
            loc: token.loc,
            name,
        }
    }

    fn parse_primary_expr(&mut self) -> AstExpr<'a> {
        // println!("---------- PARSE PRIMARY EXPR ");

        if let Some(token) = self.lexer.peek(0) {
            let name = self.lexer.get_string(token.text_start, token.text_len);
            // println!(
            //     "PRIMARY EXPR PEEKED TOKEN FOR EXPR {} Name: {}",
            //     Token::kind_name(token.token_kind),
            //     name
            // );

            let loc = token.loc;

            return match token.kind {
                Kind::Name => {
                    match name.as_str() {
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
                                if next_token.kind == Kind::OpenParen {
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
                    }
                }
                Kind::Number => {
                    let _ = self.lexer.extract_next();

                    if let Ok(ivalue) = name.parse::<i64>() {
                        return AstExpr {
                            loc,
                            kind: AstExprKind::LitInt(ivalue),
                        };
                    };
                    user_error!("{} can't convert number {} to i64", &token.loc.fmt_err(), &name);
                }
                Kind::Literal => {
                    let literal = self.parse_string_literal();
                    AstExpr {
                        loc,
                        kind: AstExprKind::LitString(literal),
                    }
                }
                Kind::OpenParen => {
                    self.lexer.expect_token_next(Kind::OpenParen);
                    let expr = self.parse_expr(Precedence::P0);
                    self.lexer.expect_token_next(Kind::CloseParen);
                    expr
                },
                Kind::Comma
                | Kind::Colon
                | Kind::AndAnd
                | Kind::Equals
                | Kind::EqualsEquals
                | Kind::GreaterEqual
                | Kind::NotEqual
                | Kind::LessThen
                | Kind::OrOr
                | Kind::CloseParen
                | Kind::OpenCurly
                | Kind::CloseCurly
                | Kind::Plus
                | Kind::Minus
                | Kind::Mult
                | Kind::Semicolon => {
                    user_error!(
                        "{} primary expression for token kind {} doesn't exist",
                        &token.loc.fmt_err(),
                        Token::kind_name(token.kind)
                    );
                }
            };
        }        
        user_error!(
            "{} expected primary expression, reached end of file",
            &self.lexer.get_location().fmt_err()
        );
    }


    fn binary_op_defs(kind: Kind ) -> BinaryOpDef {

        match kind {

            Kind::Plus          => BinaryOpDef{  kind : AstBinaryOpKind::Plus         , prec: Precedence::P2 },
            Kind::Minus         => BinaryOpDef{  kind : AstBinaryOpKind::Minus        , prec: Precedence::P2 },
            Kind::Mult          => BinaryOpDef{  kind : AstBinaryOpKind::Mult         , prec: Precedence::P3 },

            Kind::AndAnd        => BinaryOpDef{  kind : AstBinaryOpKind::AndAnd       , prec: Precedence::P0 },
            Kind::EqualsEquals  => BinaryOpDef{  kind : AstBinaryOpKind::EqualsEquals , prec: Precedence::P1 },
            Kind::GreaterEqual  => BinaryOpDef{  kind : AstBinaryOpKind::GreaterEqual , prec: Precedence::P1 },
            Kind::LessThen      => BinaryOpDef{  kind : AstBinaryOpKind::LessThen     , prec: Precedence::P1 },
            Kind::NotEqual      => BinaryOpDef{  kind : AstBinaryOpKind::NotEqual     , prec: Precedence::P1 },
            Kind::OrOr          => BinaryOpDef{  kind : AstBinaryOpKind::OrOr         , prec: Precedence::P0 },

            Kind::Name |
            Kind::Number |
            Kind::OpenParen |
            Kind::CloseParen |
            Kind::OpenCurly |
            Kind::CloseCurly |
            Kind::Semicolon |
            Kind::Literal |
            Kind::Colon |
            Kind::Equals |
            Kind::Comma => {
                unreachable!();
            }
        }
    }

                    

    fn parse_expr(&mut self, prec : Precedence ) -> AstExpr<'a> {

        //println!("---------- PARSE EXPR {:?}", prec);

        if prec == Precedence::PMax {
            return self.parse_primary_expr();
        }

        let mut lhs = self.parse_expr( Precedence::next( prec ) );

        while let Some(token) = self.lexer.peek(0) {
            //let name = self.lexer.get_string(token.text_start, token.text_len);
            // println!(
            //     "EXPR: PEEKED TOKEN FOR EXPR {} Name: {}",
            //     Token::kind_name(token.token_kind),
            //     name
            // );

            match &token.kind {
                // groep binary operators?
                Kind::AndAnd |
                Kind::OrOr   |

                Kind::LessThen |
                Kind::GreaterEqual |
                Kind::NotEqual |
                Kind::EqualsEquals |
                Kind::Mult |
                Kind::Plus |
                Kind::Minus => {

                    // token is binary operator
                    let bin_op_def = Self::binary_op_defs(token.kind);

                    // check if the precdedence of the operator
                    if bin_op_def.prec == prec {
                        self.lexer.extract_next();
                        let rhs = self.parse_expr(Precedence::next( prec ));
    
                        let kind = AstExprKind::BinarayOp(AstBinaryOp {
                            loc: token.loc,
                            kind: bin_op_def.kind,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        });
    
                        lhs = AstExpr {
                            loc: token.loc,
                            kind,
                        };
                    } else {
                        break;
                    }
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
                    break;
                }
            }
        }
        lhs
    }

    fn parse_if(&mut self) -> AstStatement<'a> {
        // println!("---------- PARSE IF STATMENT ");

        let token = self.lexer.expect_keyword("if");

        // open and close paren
        let expr = self.parse_expr(Precedence::P0);
        let then_block = Box::new(self.parse_curly_block());

        let mut else_block = None;

        if let Some(token) = self.lexer.peek(0) {
            if self.lexer.is_keyword(&token, "else") {
                self.lexer.extract_next();
                else_block = Some(Box::new(self.parse_curly_block()));
            }
        }

        AstStatement::If(AstIfStatement {
            loc: token.loc,
            condition: expr,
            then_block,
            else_block,
        })
    }

    fn parse_while(&mut self) -> AstStatement<'a> {
        // println!("---------- PARSE WHILE STATMENT ");

        let token = self.lexer.expect_keyword("while");

        // open and close paren
        let expr = self.parse_expr(Precedence::P0);
        let block = Box::new(self.parse_curly_block());

        AstStatement::While(AstWhileStatement {
            loc: token.loc,
            condition: expr,
            block,
        })
    }

    fn parse_statement(&mut self) -> AstStatement<'a> {
        // println!("---------- PARSE STATMENT ");

        if let Some(token) = self.lexer.peek(0) {
            match token.kind {
                Kind::Name => {
                    // check if statement, return no semicolon needed
                    if self.lexer.is_keyword(&token, "if") {
                        return self.parse_if();
                    }

                    // check if statement, return no semicolon needed
                    if self.lexer.is_keyword(&token, "while") {
                        return self.parse_while();
                    }

                    // check if statement, return no semicolon needed
                    if self.lexer.is_keyword(&token, "var") {
                        return AstStatement::VarDef( self.parse_var_def() );
                    }

                    // check var assignment statement, semicoln parsed inside var_assign
                    if let Some(next_token) = self.lexer.peek(1) {
                        if next_token.kind == Kind::Equals {
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
                | Kind::Comma
                | Kind::Equals
                | Kind::Plus
                | Kind::Minus
                | Kind::Mult
                | Kind::AndAnd
                | Kind::EqualsEquals
                | Kind::GreaterEqual
                | Kind::NotEqual
                | Kind::OrOr
                | Kind::LessThen
                | Kind::Semicolon => {
                    // fallthrough, parse as an expression with a semicolon
                }
            };

            // parse as exprssion with a semicolon
            // println!("---------- PARSE STATMENT AS EXPRESSION -------");
            let stmt = AstStatement::Expr(self.parse_expr(Precedence::P0));
            self.lexer.expect_token_next(Kind::Semicolon);

            // println!("STATEMENT AS EXPRESSION: {:?}", stmt);
            stmt
        } else {
            user_error!("{} expected statement, reached end of file", &self.lexer.get_location().fmt_err());
        }
    }

    fn parse_curly_block(&mut self) -> AstBlock<'a> {
        // println!("---------- CURLY BLOCK ");
        let mut stmts = Vec::new();

        // expect open curly
        self.lexer.expect_token_next(Kind::OpenCurly);

        while let Some(token) = self.lexer.peek(0) {
            if token.kind == Kind::CloseCurly {
                break;
            }
            // add expression to block
            let stmt = self.parse_statement();
            stmts.push(stmt);
        }
        self.lexer.expect_token_next(Kind::CloseCurly);

        AstBlock { statements: stmts }
    }

    fn parse_proc_params(&mut self) -> Vec<AstProcParam<'a>> {

        println!("Parse proce ");

        // open and close paren
        let mut params = Vec::<AstProcParam>::new();

        self.lexer.expect_token_next(Kind::OpenParen);

        while let Some(next_token) = self.lexer.peek(0) {
            println!("Parse proce token {:?} ", &next_token);

            match next_token.kind {
                Kind::CloseParen => {
                    self.lexer.expect_token_next(Kind::CloseParen);
                    return params;
                },
                Kind::Name => {
                    // expect name of var token
                    let token = self.lexer.expect_token_next(Kind::Name);
                    let param_name = self.lexer.get_string(token.text_start, token.text_len);
                    // expect colon
                    self.lexer.expect_token_next(Kind::Colon);

                    // expect type
                    let param_type = self.parse_type();

                    params.push(AstProcParam {
                        loc: token.loc,
                        param_name,
                        param_type,
                    })
                },
                Kind::Comma => {
                    self.lexer.expect_token_next(Kind::Comma);
                    // TODO CHECK FOR DOUBLE COMMA's
                },
                _ => {
                    user_error!(
                        "{} unexpected token {} ",
                        &self.lexer.get_location().fmt_err(),
                        Token::kind_name(next_token.kind)
                    );                    
                }
            }
        }

        user_error!(
            "{} expected ), reached end of file",
            &self.lexer.get_location().fmt_err()
        );
    }

    fn parse_proc_def(&mut self) -> AstProcDef<'a> {
        // println!("---------- PARSE PROC DEF ");

        // check proc token
        let token = self.lexer.expect_keyword("proc");
        let loc = token.loc;

        // expect name of proc token
        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);

        let params = self.parse_proc_params();

        let body = self.parse_curly_block();


        AstProcDef { loc, name, body, params }
    }

    fn parse_type(&mut self) -> AstTypes {
        // println!("---------- PARSE TYPE ");
        // expect type name
        let token = self.lexer.expect_token_next(Kind::Name);
        let type_name = self.lexer.get_string(token.text_start, token.text_len);
        //println!("VAR TYPE NAME IS: {} ", type_name);

        if let Some(type_kind) = name_to_type(type_name.as_str()) {
            return type_kind;
        }

        user_error!("{} unknown file type {}",&self.lexer.get_location().fmt_err(), &type_name);
    }

    fn parse_var_def(&mut self) -> AstVarDef<'a> {
        // println!("---------- PARSE VAR DEF");

        // check var token
        let token = self.lexer.expect_keyword("var");
        let loc = token.loc;

        // expect name of var token
        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);

        // expect colon
        self.lexer.expect_token_next(Kind::Colon);

        let var_type = self.parse_type();

        let mut init_expr = None;

        if let Some(next_token) = self.lexer.peek(0) {
            // println!("CHECK VAR ASSIGNMENT ");
            if next_token.kind == Kind::Equals {
                self.lexer.extract_next();
                init_expr = Some( self.parse_expr(Precedence::P0) );
            }
        }

        // expect semicolon
        let _ = self.lexer.expect_token_next(Kind::Semicolon);

        AstVarDef {
            loc,
            name,
            init_expr,
            var_type,
        }
    }

    fn parse_var_assign(&mut self) -> AstStatement<'a> {
        // println!("---------- PARSE VAR ASSIGN ");

        // check var token
        let token = self.lexer.expect_token_next(Kind::Name);
        let name = self.lexer.get_string(token.text_start, token.text_len);
        let loc = token.loc;
        let _ = self.lexer.expect_token_next(Kind::Equals);
        let expr = self.parse_expr(Precedence::P0);
        let _ = self.lexer.expect_token_next(Kind::Semicolon);

        AstStatement::VarAssign(AstVarAssign { loc, name, expr })
    }

    fn parse_top(&mut self, token: &Token) -> AstTop<'a> {
        // println!("---------- PARSE TOP ");

        if self.lexer.is_keyword(&token, "proc") {
            return AstTop::ProcDef(self.parse_proc_def());
        }

        if self.lexer.is_keyword(&token, "var") {
            return AstTop::VarDef(self.parse_var_def());
        }

        // the code below currently expects 2 top level variants
        // force compiler error when adding new variant
        sa::const_assert!(AstTop::VARIANT_COUNT == 2);
        user_error!(
            "{} expected var or proc, got {} ",
            &token.loc.fmt_err(),
            Token::kind_name(token.kind)
        );
    }

    fn parse_module(&mut self) -> AstModule<'a> {
        let mut tops = Vec::new();

        // while we got tokens left, parse top defs
        while let Some(token) = self.lexer.peek(0) {
            tops.push(self.parse_top(&token));
        }

        AstModule { tops }
    }

    pub fn parse(&mut self) -> AstModule<'a> {
        self.parse_module()
    }
}
