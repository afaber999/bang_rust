use crate::location::Location;

#[derive(Debug, Clone, Copy, PartialEq)]
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
    Comma,

    Plus,
    Mult,
    Minus,

    AndAnd,
    Equals,
    EqualsEquals,
    GreaterEqual,
    LessThen,
    NotEqual,
    OrOr,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub text_str : &'a str,
    pub kind: Kind,
    pub loc: Location<'a>,
}

impl<'a> Token<'a> {
    pub fn new(text_str: &'a str, token_kind: Kind, loc: Location<'a>) -> Self {
        Self {
            text_str,
            kind: token_kind,
            loc,
        }
    }

    #[must_use]
    pub fn kind_name(token_kind: Kind) -> &'static str {
        match token_kind {
            Kind::Name => "Name",
            Kind::Number => "Number",

            Kind::OpenParen => "(",
            Kind::CloseParen => ")",
            Kind::OpenCurly => "{",
            Kind::CloseCurly => "}",
            Kind::Semicolon => ";",
            Kind::Literal => "Literal",
            Kind::Colon => ":",
            Kind::Comma => ",",
            
            Kind::Plus => "+",
            Kind::Minus => "-",
            Kind::Mult => "*",
            
            Kind::AndAnd => "&&",
            Kind::Equals => "=",
            Kind::EqualsEquals => "==",
            Kind::GreaterEqual => ">=",
            Kind::LessThen => "<",
            Kind::NotEqual => "!=",
            Kind::OrOr   => "||",

        }
    }
    
    pub fn get_symbol_tokens() -> &'static [Kind] {
        // note the order is important, the longest symbols must be listed first!
        &[
            // two characters tokens
            Kind::AndAnd,
            Kind::EqualsEquals,
            Kind::GreaterEqual,
            Kind::NotEqual,
            Kind::OrOr,

            // single characters tokens
            Kind::OpenParen,
            Kind::CloseParen,
            Kind::OpenCurly,
            Kind::CloseCurly,
            Kind::Semicolon,
            Kind::Colon,
            Kind::Equals,
            Kind::Plus,
            Kind::Minus,
            Kind::LessThen,
            Kind::Comma,
            Kind::Mult,       
        ]
    }    
}

