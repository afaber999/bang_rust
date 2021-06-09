use std::{usize};
use crate::location::Location;
use crate::token::Token;
use crate::token::TokenType;


pub struct Lexer {
    content    : Vec<char>,
    cur_idx    : usize,
    last_idx   : usize,

    line_start : usize,
    line_end   : usize,
    row        : usize,

    file_idx   : usize,
}

impl Lexer {
    pub fn new( content : String, file_idx : usize ) -> Self {
       
        let mut result = Self {
            content   : content.chars().collect(),
            cur_idx   : 0,
            last_idx  : 0,
            line_start: 0,
            line_end  : 0,
            row       : 0,
            file_idx,
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
        true    
    }

    pub fn dump(&self) {
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

    pub fn get_string(&mut self,  text_start:usize, text_len:usize) -> String {
        self.content[text_start..text_start+text_len].into_iter().collect()
    }
 
    fn get_location(&self) -> Location {
        Location {
            row: self.row,
            col: self.cur_idx - self.line_start,
            file_idx : self.file_idx,                
        }
    }

    fn extract_token(&mut self, token_type:TokenType, token_size:usize) ->Token {

        assert!(self.line_len() >= token_size);

        let result = Token::new( 
            token_type,
            self.cur_idx, 
            token_size, 
            self.get_location() );

        self.cur_idx+= token_size;
        result
    }

    pub fn next(&mut self) -> Option<Token>{

        while self.is_line_empty() {
            self.next_line();
            if self.eos() {
                return None;
            } 
            //self.dump();
        }

        // eat all white spaces
        self.trim();


        let mut hardcoded_tokens  = std::collections::HashMap::new();
        hardcoded_tokens.insert("(", TokenType::OpenParen );
        hardcoded_tokens.insert(")", TokenType::CloseParen );
        hardcoded_tokens.insert("{", TokenType::OpenCurly );
        hardcoded_tokens.insert("}", TokenType::CloseCurly );
        hardcoded_tokens.insert(";", TokenType::Semicolon );

        for (token_text, token_type) in hardcoded_tokens.iter() {
            if self.starts_with( &token_text ) {
                return Some( self.extract_token(*token_type, token_text.len()) );
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
            return Some( self.extract_token(TokenType::Name, token_len ));
        }

        // string literal
        if self.content[ self.cur_idx] == '\"' {
            let mut pos = self.cur_idx + 1;

            while pos < self.line_end && self.content[pos] != '\"' {
                pos += 1;                
            }

            if pos < self.line_end {
                let token_len = pos - self.cur_idx+ 1;
                let token = self.extract_token(TokenType::Literal, token_len);
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
}

