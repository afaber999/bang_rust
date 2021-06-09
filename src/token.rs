use crate::location::Location;
use enum_iterator::IntoEnumIterator;

#[derive(Debug, Clone, Copy, IntoEnumIterator)]
pub enum TokenType {
    Name,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Semicolon,
    Literal,
}


pub fn token_type_name(token_type : TokenType ) -> &'static str {
    match token_type {
        TokenType::Name => "Name",
        TokenType::OpenParen => "OpenParen",
        TokenType::CloseParen => "CloseParen",
        TokenType::OpenCurly => "OpenCurly",
        TokenType::CloseCurly => "CloseCurly",
        TokenType::Semicolon => "Semicolon",
        TokenType::Literal => "Literal",
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type : TokenType,
    pub text_start : usize,
    pub text_len   : usize,
    pub loc : Location,
}

impl Token {
    pub fn new( token_type: TokenType, text_start: usize, text_len: usize , loc: Location)->Self {
        Self {
            token_type,
            text_start,
            text_len,
            loc,
        }
    }
}

