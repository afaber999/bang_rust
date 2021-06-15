use crate::location::Location;
use enum_iterator::IntoEnumIterator;

#[derive(Debug, Clone, Copy, IntoEnumIterator, PartialEq)]
pub enum TokenKind {
    Name,
    Number,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Semicolon,
    Literal,
    Colon,
    Equals,
}


pub fn token_kind_name(token_kind : TokenKind ) -> &'static str {
    match token_kind {
        TokenKind::Name => "Name",
        TokenKind::Number => "Number",
        TokenKind::OpenParen => "OpenParen",
        TokenKind::CloseParen => "CloseParen",
        TokenKind::OpenCurly => "OpenCurly",
        TokenKind::CloseCurly => "CloseCurly",
        TokenKind::Semicolon => "Semicolon",
        TokenKind::Literal => "Literal",
        TokenKind::Colon => ":",
        TokenKind::Equals => "=",
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_kind : TokenKind,
    pub text_start : usize,
    pub text_len   : usize,
    pub loc : Location,
}

impl Token {
    pub fn new( token_kind: TokenKind, text_start: usize, text_len: usize , loc: Location)->Self {
        Self {
            token_kind,
            text_start,
            text_len,
            loc,
        }
    }
}

