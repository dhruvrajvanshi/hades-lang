use std::todo;

use super::token::{Token, TokenKind, TokenKind as t};
use crate::core::{SourceLocation, SourcePath, SourcePosition};

pub struct Lexer<'a> {
    source_path: SourcePath,
    input: &'a str,
    location: SourceLocation,
    current_text: String,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, source_path: SourcePath) -> Lexer<'a> {
        Lexer {
            source_path,
            input,
            current_text: String::new(),
            location: SourceLocation {
                source_path,
                start: SourcePosition { line: 1, column: 1 },
                stop: SourcePosition { line: 1, column: 1 },
            },
        }
    }
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start_token();
        match self.current_char() {
            '\0' => self.make_token(t::End),
            '.' => self.punctuation(t::Dot),
            '(' => self.punctuation(t::LParen),
            ')' => self.punctuation(t::RParen),
            '{' => self.punctuation(t::LBrace),
            '}' => self.punctuation(t::RBrace),
            ',' => self.punctuation(t::Comma),
            ':' => self.punctuation(t::Colon),
            '*' => self.punctuation(t::Star),
            ';' => {
                self.advance();
                self.make_token(t::Semicolon)
            }
            '=' => {
                self.advance();
                if self.current_char() == '=' {
                    self.advance();
                    self.make_token(t::EqualEqual)
                } else {
                    self.make_token(t::Equal)
                }
            }
            c if c == 'b' && self.next_char() == '"' => self.byte_string(),
            c if is_identifier_starter(c) => self.identifier_or_keyword(),
            _ => {
                todo!(
                    "{:?}: Unexpected char {}",
                    self.location,
                    self.current_char()
                )
            }
        }
    }

    fn byte_string(&mut self) -> Token {
        self.advance();
        self.advance();

        while self.current_char() != '"' && self.current_char() != '\0' {
            self.advance();
        }
        self.advance();
        self.make_token(t::BinaryStringLiteral)
    }

    fn next_char(&self) -> char {
        self.input.chars().nth(1).unwrap_or('\0')
    }

    fn punctuation(&mut self, kind: TokenKind) -> Token {
        self.advance();
        self.make_token(kind)
    }

    fn identifier_or_keyword(&mut self) -> Token {
        self.advance();
        while is_identifier_char(self.current_char()) {
            self.advance();
        }
        self.make_token(match self.current_text.as_str() {
            "import" => t::Import,
            "extern" => t::Extern,
            "def" => t::Def,
            "as" => t::As,
            "return" => t::Return,
            "mut" => t::Mut,
            _ => t::Identifier,
        })
    }

    fn advance(&mut self) -> char {
        if self.current_char() == '\0' {
            panic!("Tried to advance past end of file");
        }
        let char = self.current_char();
        self.current_text.push(char);

        if char == '\n' {
            self.location.stop.line += 1;
            self.location.stop.column = 1;
        } else {
            self.location.stop.column += 1;
        }
        let next_input = &self.input[1..];
        self.input = next_input;
        char
    }

    fn skip_whitespace(&mut self) {
        while self.current_char().is_whitespace() {
            self.advance();
        }
    }

    fn start_token(&mut self) {
        self.current_text = String::new();
        self.location.start = self.location.stop;
    }

    fn current_char(&self) -> char {
        if self.input.is_empty() {
            '\0'
        } else {
            self.input.chars().next().unwrap()
        }
    }

    fn make_token(&mut self, kind: TokenKind) -> Token {
        Token {
            location: self.location,
            kind,
            text: std::mem::replace(&mut self.current_text, String::new()),
        }
    }
}

fn is_identifier_starter(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_identifier_char(c: char) -> bool {
    c.is_digit(10) || is_identifier_starter(c)
}
