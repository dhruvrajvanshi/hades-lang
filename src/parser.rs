use std::array;
use std::{path::PathBuf, rc::Rc};

use crate::ast::{
    Block, Expr, ExprKind, Fn, Item, ItemKind, NodeId, SourceFile, Stmt, StmtKind, Ty, TyKind,
};
use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser<'text> {
    path: Rc<PathBuf>,
    tokens: TokenBuffer<'text>,
}

use TokenKind as t;
impl<'text> Parser<'text> {
    pub fn new(text: &'text str, path: PathBuf) -> Self {
        let lexer = Lexer::new(text, path.clone());
        let tokens = TokenBuffer::new(lexer);
        Parser {
            path: Rc::new(path),
            tokens,
        }
    }

    pub fn parse_source_file(mut self) -> SourceFile {
        let mut items = vec![];
        while self.current_kind() != TokenKind::EOF {
            items.push(self.parse_item());
        }
        SourceFile {
            path: PathBuf::clone(&self.path),
            items,
        }
    }

    fn parse_item(&mut self) -> Item {
        use t::*;
        match self.current_kind() {
            FN => Item {
                id: self.node_id(),
                kind: ItemKind::Fn(Box::new(self.parse_fn())),
            },
            _ => todo!(),
        }
    }

    fn parse_fn(&mut self) -> Fn {
        self.expect(TokenKind::FN, "Trying to parse function");
        let name = self.expect(TokenKind::IDENT, "fn [name]").text;
        self.expect(TokenKind::LPAREN, "Expected parameter list start");
        self.expect(TokenKind::RPAREN, "Expected parameter list end");
        let return_ty = if self.current_kind() == TokenKind::ARROW {
            self.advance();
            Some(self.parse_ty())
        } else {
            None
        };
        let body = self.parse_block();
        Fn {
            id: self.node_id(),
            name,
            body,
            return_ty,
        }
    }

    fn parse_block(&mut self) -> Expr {
        self.expect(TokenKind::LBRACE, "Trying to parse block");
        let mut stmts = vec![];
        while self.current_kind() != TokenKind::RBRACE && !self.eof() {
            stmts.push(self.parse_stmt());
        }

        self.expect(
            TokenKind::RBRACE,
            "Unexpected end of file while looking for block terminator",
        );
        Expr {
            id: self.node_id(),
            kind: ExprKind::Block(Block {
                id: self.node_id(),
                stmts,
            }),
        }
    }

    fn parse_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr();
        Stmt {
            id: self.node_id(),
            kind: StmtKind::Expr(Box::new(expr)),
        }
    }

    fn parse_expr(&mut self) -> Expr {
        match self.current_kind() {
            TokenKind::LPAREN => {
                self.advance();
                self.expect(TokenKind::RPAREN, "Trying to parse unit expression");
                Expr {
                    id: self.node_id(),
                    kind: ExprKind::Unit,
                }
            }
            _ => todo!(),
        }
    }

    fn parse_ty(&mut self) -> Ty {
        self.expect(TokenKind::LPAREN, "Trying to parse unit type");
        self.expect(TokenKind::RPAREN, "Trying to parse end of unit type");
        Ty {
            id: self.node_id(),
            kind: TyKind::Unit,
        }
    }

    fn eof(&self) -> bool {
        self.current_kind() == TokenKind::EOF
    }

    fn node_id(&mut self) -> NodeId {
        NodeId(0)
    }

    fn expect(&mut self, kind: TokenKind, message: &str) -> Token {
        let token = self.advance();
        if token.kind != kind {
            panic!(
                "Parse error: {}\nExpected: {:?}; Found: '{}';\nTokenBuffer: {:?}",
                message, kind, token.text, self.tokens
            )
        }
        token
    }

    fn current_kind(&self) -> TokenKind {
        self.tokens.current_kind()
    }

    fn current(&self) -> &Token {
        self.tokens.current()
    }
    fn advance(&mut self) -> Token {
        self.tokens.advance()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_empty_function() {
        let path = PathBuf::from("test.hds");
        let parser = Parser::new("fn main() -> () {}", path);
        parser.parse_source_file();
    }
}

const MAX_LOOKAHEAD: usize = 4;
/// A ring buffer of N tokens; Allows peeking and consuming tokens.
#[derive(Debug)]
struct TokenBuffer<'text> {
    start: usize,
    tokens: [Token; MAX_LOOKAHEAD],
    lexer: Lexer<'text>,
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

    pub fn current(&self) -> &Token {
        self.peek(0)
    }

    pub fn current_kind(&self) -> TokenKind {
        self.current().kind
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

    #[test]
    fn test_overflow() {
        let lexer = Lexer::new("fn main() {}", PathBuf::from("test"));
        let mut buffer = TokenBuffer::new(lexer);

        use TokenKind::*;
        assert_eq!(buffer.advance().kind, FN);
        assert_eq!(buffer.advance().kind, IDENT);
        assert_eq!(buffer.advance().kind, LPAREN);
        assert_eq!(buffer.advance().kind, RPAREN);
        assert_eq!(buffer.advance().kind, LBRACE);
        assert_eq!(buffer.advance().kind, RBRACE);
    }
}
