use std::{usize};
use crate::location::FileNameLocations;
use crate::location::Location;
use crate::location::fmt_loc_err;
use crate::token::Token;
use crate::token::TokenKind;
use crate::token::token_kind_name;

#[derive(Debug)]
pub struct Lexer<'a> {
    content    : Vec<char>,
    cur_idx    : usize,
    last_idx   : usize,

    line_start : usize,
    line_end   : usize,
    row        : usize,

    file_idx   : usize,
    filename_locations : &'a FileNameLocations,
    peek_token : Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new( content : String, input_file_name : String, filename_locations : &'a FileNameLocations ) -> Self {
       
        // add to fileame locations
        let file_idx =  filename_locations.insert(input_file_name);

        let mut result = Self {
            content   : content.chars().collect(),
            cur_idx   : 0,
            last_idx  : 0,
            line_start: 0,
            line_end  : 0,
            row       : 0,
            file_idx,
            filename_locations,
            peek_token:None
        };
        result.set_line_end();
        //result.dump();
        result
    }

    fn is_line_empty(&self)-> bool {
        for pos in self.cur_idx..self.line_end {
            let ch = self.content[pos];
            if !ch.is_whitespace() && ch !='\r' {
                return false;
            }
        }
        true
    }

    fn trim_start(&mut self) {
        for pos in self.cur_idx..self.line_end {
            let ch = self.content[pos];
            //println!("Trim_start Checking {} {}", pos, ch);
            if !ch.is_whitespace() && ch !='\r' {
                self.cur_idx = pos;
                return;
            }
        }
        self.cur_idx = self.line_end;
    }

    fn trim_end(&mut self) {
        for pos in (self.cur_idx..self.line_end ).rev() {
            let ch = self.content[pos];
            //println!("Checking {} {}", pos, ch);
            if !ch.is_whitespace() && ch !='\r'  {
                self.last_idx = pos + 1;
                return;
            }
        }
        self.last_idx = self.line_end;
    }

    fn trim(&mut self) {
        self.trim_start();
        self.trim_end();
    }

    fn line_len(&self) -> usize {
        self.last_idx - self.cur_idx
    }

    fn starts_with(&self, value:&str)-> bool {
        if self.line_len() < value.len() {
            return false;
        }
        for (i,c ) in value.chars().enumerate() {
            if self.content[self.cur_idx+i] != c {
                return false;
            }
        }
        println!("STARTSWITH: {}", &value);

        true    
    }

    pub fn dump(&self) {
        println!("-------------------- DUMP LEXER --------------------");
        println!("LINE      :{}", self.get_string(self.line_start, self.line_len()));
        println!("line_start:{:?}", self.line_start);
        println!("line_end  :{:?}", self.line_end);
        println!("last_idx  :{:?}", self.last_idx);
        println!("first_char:{:?}", self.content[self.line_start]);
        println!("last_char :{:?}", self.content[self.line_end]);

    }

    pub fn next_line(&mut self) {
        self.row += 1;
        self.line_start = self.line_end + 1;
        self.set_line_end();
        self.cur_idx = self.line_start;

        //     println!("Reached end of stream: " );
        //     return;
        // }
        // println!("After next line: " );
        // self.dump();
        // if self.is_line_empty() {
        //     println!("LINE IS EMPTY: " );
        // }
    }

    pub fn eos(&self) -> bool {
        self.line_start >= self.content.len()
    }

    fn set_line_end(&mut self) {
        self.line_end = self.line_start;
        while self.line_end < self.content.len()-1 {
            if self.content[self.line_end] == '\n' {
                break;
            }
            self.line_end += 1;
        } 
        self.last_idx = self.line_end;
    }

    pub fn is_name(ch:char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }
    pub fn is_number(ch:char) -> bool {
        ch.is_alphanumeric() || ch == '.'
    }

    pub fn get_string(&self,  text_start:usize, text_len:usize) -> String {
        self.content[text_start..text_start+text_len].into_iter().collect()
    }
    pub fn get_char(&mut self,  index:usize) -> char {
        self.content[index]
    }


    pub fn get_location(&self) -> Location {
        Location {
            row: self.row,
            col: self.cur_idx - self.line_start,
            file_idx : self.file_idx,                
        }
    }

    fn extract_token(&mut self, token_kind:TokenKind, token_size:usize) ->Token {

        assert!(self.line_len() >= token_size);

        let result = Token::new( 
            token_kind,
            self.cur_idx, 
            token_size, 
            self.get_location() );

        self.cur_idx+= token_size;
        result
    }

    pub fn peek(&mut self) -> Option<Token>{
        self.peek_token = self.next();
        //println!( "PEEK TOKEN KIND: {:?}", self.peek_token);
        self.peek_token        
    }


    pub fn next(&mut self) -> Option<Token>{
        // works for now, need to refactor later
        if self.peek_token.is_some() {
            let token = self.peek_token;
            self.peek_token = None;
            //println!( "NEXT TOKEN KIND: {:?}", &token);
            return token;
        }

        println!("CHECK LINE: ");

        loop {
            // eat all white spaces at begin and end of current line
            self.trim();

            // skip comments and empty lines
            if self.is_line_empty() || self.starts_with("#") {
                // fetch next line
                self.next_line();

                // make sure we did not reach end of stream
                if self.eos() {
                    return None;
                } 
                continue;
            }
            // use line for next token
            break;
        }

        let mut hardcoded_tokens  = std::collections::HashMap::new();
        hardcoded_tokens.insert("(", TokenKind::OpenParen );
        hardcoded_tokens.insert(")", TokenKind::CloseParen );
        hardcoded_tokens.insert("{", TokenKind::OpenCurly );
        hardcoded_tokens.insert("}", TokenKind::CloseCurly );
        hardcoded_tokens.insert(";", TokenKind::Semicolon );
        hardcoded_tokens.insert(":", TokenKind::Colon );

        for (token_text, token_kind) in hardcoded_tokens.iter() {
            if self.starts_with( &token_text ) {
                let opt_token =  Some( self.extract_token(*token_kind, token_text.len()) );
                //println!( "NEXT TOKEN KIND: {:?}", &opt_token);
                return opt_token
            }
        }

        // name tokens
        let mut token_len = 0;
        for pos in self.cur_idx..self.line_end {
            if !Lexer::is_name( self.content[pos] ) {
                break;
            }
            token_len += 1;
        }
        if token_len > 0  {
            let opt_token =  Some( self.extract_token(TokenKind::Name, token_len ));
            //println!( "NEXT TOKEN KIND: {:?}", &opt_token);
            return opt_token
    }

        // string literal
        if self.content[ self.cur_idx] == '\"' {
            let mut pos = self.cur_idx + 1;

            while pos < self.line_end && self.content[pos] != '\"' {
                pos += 1;                
            }

            if pos < self.line_end {
                let token_len = pos - self.cur_idx+ 1;
                let token = self.extract_token(TokenKind::Literal, token_len);
                //println!( "NEXT TOKEN KIND: {:?}", &token);
                return Some( token );
            } else {
                panic!("Missing closing character on string literal");
            }
    
        } 


        self.dump();
        panic!("Cur idx is {} ch is {}", self.cur_idx , self.content[self.cur_idx]);
        // Some( Token::new( 
        //     TokenType::Name,
        //     self.cur_idx, 
        //     self.line_len(), 
        //     self.get_location() ) )
    }

    pub fn is_keyword(&mut self, token: &Token, keyword : &str) -> bool {
        if token.token_kind != TokenKind::Name {
            return false
        }
        let token_name = self.get_string( token.text_start, token.text_len );
        return token_name == keyword
    }

    pub fn expect_keyword(&mut self, name: &str) -> Token {

        let token = self.expect_token_next(TokenKind::Name);
        let token_name = self.get_string( token.text_start, token.text_len );
        
        if token_name != name {
            let loc_msg = fmt_loc_err( self.filename_locations, &token.loc);
            user_error!("{} expect keyword '{}' but got '{}'", loc_msg, name, token_name);
        }

        token
    }

    pub fn expect_token_next(&mut self, token_kind : TokenKind) -> Token {

        if let Some( token ) = self.next() {
            if token.token_kind != token_kind {
                let loc_msg = fmt_loc_err( self.filename_locations, &token.loc);
                user_error!("{} Expected token {} but got {}",
                    loc_msg,
                    token_kind_name(token_kind),
                    token_kind_name(token.token_kind));

            }
            return token;
        } 

        let loc_msg = fmt_loc_err( self.filename_locations, &self.get_location());
        user_error!("{} reached end of input, expected token type: {}", loc_msg, token_kind_name(token_kind));
    }
}

