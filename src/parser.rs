use crate::{lexer::Lexer, location::{FileNameLocations, fmt_loc_err}, token::{TokenKind, token_kind_name}};


#[derive(Debug)]
pub struct AstFunCall {
    pub name : String,
    pub args : String,
}

#[derive(Debug)]
pub enum AstExprKind {
    Empty,
    FuncCall(AstFunCall),
    LitFloat(f64),
    LitInt(i64),
    LitChar(char),
    LitString(String),
}

impl Default for AstExprKind {
    fn default() -> Self { AstExprKind::Empty }
}

#[derive(Debug, Default)]
pub struct AstExpr {
    pub kind : AstExprKind,
}


#[derive(Debug, Default)]
pub struct AstStatement{
    pub expr : AstExpr,
} 

#[derive(Debug, Default)]
pub struct AstBlock{
    pub statements : Vec<AstStatement>,
} 

#[derive(Debug, Default)]
pub struct AstProcDef{
    pub name : String,
    pub body : AstBlock
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


    fn parse_expr(&mut self) -> AstStatement {

        if let Some( token ) = self.lexer.next() {
            let name = self.lexer.get_string(token.text_start, token.text_len);
            println!("GOT TOKEN FOR EXPR {} Name: {}", token_kind_name(token.token_type), name);
        }
        AstStatement::default()
    }

    fn parse_curly_block(&mut self) -> AstBlock {

        let mut block = AstBlock::default();

        // expect open curly
        self.lexer.expect_token_next(TokenKind::OpenCurly);


        while let Some(token) = self.lexer.peek() {
            if token.token_type == TokenKind::CloseCurly {
                break;
            }

            // add expression to block
            let expr = self.parse_expr();
            block.statements.push( expr );

        }
        self.lexer.expect_token_next(TokenKind::CloseCurly);


        block
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