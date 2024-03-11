use core::panic;
use std::{collections::HashMap, path::PathBuf, rc::Rc, str::Chars};

use lazy_static::lazy_static;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TokenKind {
    FN,
    IDENT,

    // Punctuation
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    EOF,
}
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub path: Rc<PathBuf>,
    pub start_position: usize,
    pub text: String,
}

lazy_static! {
    static ref TOKEN_KINDS: HashMap<&'static str, TokenKind> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenKind::FN);
        m
    };
}

lazy_static! {
    static ref SINGLE_CHAR_TOKENS: HashMap<char, TokenKind> = {
        let mut m = HashMap::new();
        let mut i = |k, v| m.insert(k, v);
        use TokenKind as t;
        i('(', t::LPAREN);
        i(')', t::RPAREN);
        i('{', t::LBRACE);
        i('}', t::RBRACE);
        m
    };
}

pub(crate) struct Lexer<'chars> {
    current_char: char,
    text: Chars<'chars>,
    lexeme: String,
    path: Rc<PathBuf>,
    position: usize,
}
impl<'chars> Lexer<'chars> {
    pub fn new(text: &'chars str, path: PathBuf) -> Self {
        let mut chars = text.chars();
        let current_char = chars.next().unwrap_or('\0');
        Lexer {
            current_char,
            text: chars,
            path: Rc::new(path),
            lexeme: String::new(),
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start_token();
        match self.current_char {
            '\0' => self.make_token(TokenKind::EOF),
            c if is_ident_starter(c) => self.ident_or_keyword(),
            c if SINGLE_CHAR_TOKENS.contains_key(&c) => {
                self.advance();
                self.make_token(
                    *SINGLE_CHAR_TOKENS
                        .get(&c)
                        .expect("Should not panic because of `contains_key` check above"),
                )
            }
            _ => todo!(),
        }
    }

    fn ident_or_keyword(&mut self) -> Token {
        assert!(is_ident_starter(self.current_char));
        while is_ident_char(self.current_char) {
            self.advance();
        }
        self.make_token(
            *TOKEN_KINDS
                .get(self.lexeme.as_str())
                .unwrap_or(&TokenKind::IDENT),
        )
    }

    fn start_token(&mut self) {
        self.lexeme = String::new();
    }

    fn make_token(&mut self, kind: TokenKind) -> Token {
        let text = std::mem::replace(&mut self.lexeme, String::new());
        Token {
            kind,
            path: Rc::clone(&self.path),
            start_position: self.position - text.len(),
            text,
        }
    }
    fn skip_whitespace(&mut self) {
        while self.current_char.is_whitespace() && !self.eof() {
            self.advance();
        }
    }

    fn eof(&self) -> bool {
        self.current_char == '\0'
    }
    fn advance(&mut self) -> char {
        if self.eof() {
            panic!("Tried to advance past EOF")
        }
        let current_char = self.current_char;
        self.current_char = self.text.next().unwrap_or('\0');
        self.lexeme.push(current_char);
        self.position += 1;
        current_char
    }
}

fn is_ident_starter(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}
fn is_ident_char(c: char) -> bool {
    is_ident_starter(c) || c.is_numeric()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer() {
        let path = PathBuf::from("test.hds");
        let mut lexer = Lexer::new("", path);

        let token = lexer.next_token();

        assert_eq!(token.kind, TokenKind::EOF);
    }

    #[test]
    fn test_lexer_skips_whitespace() {
        let path = PathBuf::from("test.hds");
        let text = "  \t\n";
        let mut lexer = Lexer::new(text, path);

        let token = lexer.next_token();

        assert_eq!(token.kind, TokenKind::EOF);
        assert_eq!(token.start_position, 4);
    }

    #[test]
    fn test_tokenizes_fn() {
        let path = PathBuf::from("test.hds");
        let text = "  fn main";
        let mut lexer = Lexer::new(text, path);
        let mut token = lexer.next_token();

        assert_eq!(token.text, "fn");
        assert_eq!(token.kind, TokenKind::FN);
        assert_eq!(token.start_position, 2);

        token = lexer.next_token();

        assert_eq!(token.kind, TokenKind::IDENT);
        assert_eq!(token.text, "main");

        assert_eq!(lexer.next_token().kind, TokenKind::EOF);
    }

    #[test]
    fn tokenizes_parens() {
        let mut t = mk_tokenizer("(){};");
        use TokenKind as k;
        assert_eq!(t.next_token().kind, k::LPAREN);
        assert_eq!(t.next_token().kind, k::RPAREN);
        assert_eq!(t.next_token().kind, k::LBRACE);
        assert_eq!(t.next_token().kind, k::RBRACE);
    }

    fn mk_tokenizer(s: &str) -> Lexer<'_> {
        Lexer::new(s, PathBuf::from("test.hds"))
    }
}
