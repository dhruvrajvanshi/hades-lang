use std::array;
use std::{path::PathBuf, rc::Rc};

use crate::ast::SourceFile;
use crate::lexer::{Lexer, Token, TokenKind};

pub(self) struct Parser<'text> {
    path: Rc<PathBuf>,
    lexer: Lexer<'text>,
}

impl<'text> Parser<'text> {
    pub fn new(text: &'text str, path: PathBuf) -> Self {
        let lexer = Lexer::new(text, path.clone());
        Parser {
            path: Rc::new(path),
            lexer,
        }
    }

    fn parse_source_file() -> SourceFile {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_empty_function() {
        let path = PathBuf::from("test.hds");
        let parser = Parser::new("fn main() -> () {}", path);
        todo!()
    }
}

const MAX_LOOKAHEAD: usize = 4;
/// A ring buffer of N tokens; Allows peeking and consuming tokens.
struct TokenBuffer<'text> {
    lexer: Lexer<'text>,
    tokens: [Token; MAX_LOOKAHEAD],
    start: usize,
}
impl<'text> TokenBuffer<'text> {
    pub fn new(mut lexer: Lexer<'text>) -> Self {
        let tokens = array::from_fn::<Token, MAX_LOOKAHEAD, _>(|_| lexer.next_token());
        TokenBuffer {
            lexer,
            tokens,
            start: 0,
        }
    }

    pub fn peek(&self, offset: usize) -> &Token {
        assert!(offset < MAX_LOOKAHEAD);
        &self.tokens[(self.start + offset) % MAX_LOOKAHEAD]
    }

    pub fn advance(&mut self) -> Token {
        let t = self.lexer.next_token();
        // before
        // 4 1 2 3
        //   ^
        // after
        // 4 t 2 3
        //     ^
        let old = std::mem::replace(&mut self.tokens[self.start], t);
        self.start = (self.start + 1) % MAX_LOOKAHEAD;
        old
    }
}

#[cfg(test)]
mod token_buffer_test {
    use std::path::PathBuf;

    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn test_token_buffer() {
        let lexer = Lexer::new("fn main() {}", PathBuf::from("test"));
        let mut buffer = TokenBuffer::new(lexer);

        assert_eq!(buffer.peek(0).kind, TokenKind::FN);
        assert_eq!(buffer.peek(1).kind, TokenKind::IDENT);
        assert_eq!(buffer.peek(2).kind, TokenKind::LPAREN);
        assert_eq!(buffer.peek(3).kind, TokenKind::RPAREN);

        assert_eq!(buffer.advance().kind, TokenKind::FN);
        assert_eq!(buffer.peek(0).kind, TokenKind::IDENT);
    }
}
