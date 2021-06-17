use crate::location::Location;
use enum_iterator::IntoEnumIterator;

#[derive(Debug, Clone, Copy, IntoEnumIterator, PartialEq)]
pub enum Kind {
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
    Plus,
    Less,
    Comma,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_kind: Kind,
    pub text_start: usize,
    pub text_len: usize,
    pub loc: Location,
}

impl Token {
    pub fn new(token_kind: Kind, text_start: usize, text_len: usize, loc: Location) -> Self {
        Self {
            token_kind,
            text_start,
            text_len,
            loc,
        }
    }

    #[must_use]
    pub fn kind_name(token_kind: Kind) -> &'static str {
        match token_kind {
            Kind::Name => "Name",
            Kind::Number => "Number",
            Kind::OpenParen => "OpenParen",
            Kind::CloseParen => "CloseParen",
            Kind::OpenCurly => "OpenCurly",
            Kind::CloseCurly => "CloseCurly",
            Kind::Semicolon => "Semicolon",
            Kind::Literal => "Literal",
            Kind::Colon => ":",
            Kind::Equals => "=",
            Kind::Plus => "+",
            Kind::Less => "<",
            Kind::Comma => ",",
        }
    }    
}
