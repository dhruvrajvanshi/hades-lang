use crate::core::{HasLocation, SourceLocation};

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub location: SourceLocation,
    pub text: String,
}
impl HasLocation for Token {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Error,
    End,

    Identifier,
    BinaryStringLiteral,

    Extern,
    Def,
    Import,
    As,
    Return,
    Mut,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Dot,
    Colon,
    Semicolon,
    Comma,
    Star,

    Equal,
    EqualEqual,
}
