use crate::location::Location;
use enum_iterator::IntoEnumIterator;

#[derive(Debug, Clone, Copy, IntoEnumIterator, PartialEq)]
pub enum TokenKind {
    Name,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Semicolon,
    Literal,
}


pub fn token_kind_name(token_type : TokenKind ) -> &'static str {
    match token_type {
        TokenKind::Name => "Name",
        TokenKind::OpenParen => "OpenParen",
        TokenKind::CloseParen => "CloseParen",
        TokenKind::OpenCurly => "OpenCurly",
        TokenKind::CloseCurly => "CloseCurly",
        TokenKind::Semicolon => "Semicolon",
        TokenKind::Literal => "Literal",
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_type : TokenKind,
    pub text_start : usize,
    pub text_len   : usize,
    pub loc : Location,
}

impl Token {
    pub fn new( token_type: TokenKind, text_start: usize, text_len: usize , loc: Location)->Self {
        Self {
            token_type,
            text_start,
            text_len,
            loc,
        }
    }
}

