use std::todo;

use super::lexer::Lexer;
use crate::{
    core::{HasLocation, SourceLocation},
    syntax::{Declaration, SourceFile},
};

use crate::core::SourcePath;
use crate::parser::token::TokenKind as t;
use crate::parser::token::{Token, TokenKind};
use crate::syntax::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, source_path: SourcePath) -> Parser<'a> {
        let mut lexer = Lexer::new(input, source_path);
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    pub fn parse_source_file(&mut self) -> SourceFile {
        let first_token = self.current_token().location();
        let mut declarations = vec![];
        while !self.at(t::End) {
            declarations.push(self.parse_declaration())
        }
        let last_token = self.current_token();
        SourceFile {
            location: SourceLocation::between(&first_token, last_token),
            declarations,
        }
    }

    fn parse_declaration(&mut self) -> Declaration {
        match self.current_token().kind {
            t::Import => self.parse_import_as_declaration(),
            t::Extern => self.parse_extern_function_def(),
            t::Def => self.parse_function_def(),
            kind => todo!("{:?}:{:?}", self.current_token().location(), kind),
        }
    }

    fn parse_function_def(&mut self) -> Declaration {
        let signature = self.parse_function_signature();
        let body = self.parse_block();
        Declaration {
            location: SourceLocation::between(&signature, &body),
            kind: DeclarationKind::FunctionDef(FunctionDef { signature, body }),
        }
    }

    fn parse_block(&mut self) -> Block {
        let start = self.expect(t::LBrace);
        let mut statements = vec![];
        while !self.at(t::End) && !self.at(t::RBrace) {
            statements.push(self.parse_statement());
        }
        let stop = self.expect(t::RBrace);
        Block {
            location: SourceLocation::between(&start, &stop),
            statements,
        }
    }

    fn parse_statement(&mut self) -> Statement {
        match self.current_token().kind {
            t::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expression = Box::new(self.parse_expression());
        self.expect(t::Semicolon);
        Statement {
            location: expression.location(),
            kind: StatementKind::Expression(expression),
        }
    }

    fn parse_return_statement(&mut self) -> Statement {
        let start = self.expect(t::Return);
        let value = if self.expression_predicted() {
            Some(self.parse_expression())
        } else {
            None
        };
        self.expect(t::Semicolon);
        Statement {
            location: SourceLocation::between_optional(&start, &start, &value),
            kind: StatementKind::Return(value),
        }
    }

    fn parse_expression(&mut self) -> Expression {
        let head = match self.current_token().kind {
            t::Identifier => self.parse_var_expression(),
            t::BinaryStringLiteral => self.parse_binary_string_literal(),
            _ => todo!(
                "{:?}: {:?}",
                self.current_token().location(),
                self.current_token().kind
            ),
        };

        self.parse_expression_tail(head)
    }

    fn parse_binary_string_literal(&mut self) -> Expression {
        let tok = self.expect(t::BinaryStringLiteral);
        Expression {
            location: tok.location(),
            kind: ExpressionKind::BinaryStringLiteral(tok.text),
        }
    }

    fn parse_expression_tail(&mut self, head: Expression) -> Expression {
        match self.current_token().kind {
            t::LParen => self.parse_function_call(head),
            _ => head,
        }
    }

    fn parse_function_call(&mut self, callee: Expression) -> Expression {
        self.expect(t::LParen);
        let args = self.parse_comma_separated_list(t::RParen, |p| p.parse_arg());
        let rparen = self.expect(t::RParen);

        Expression {
            location: SourceLocation::between(&callee, &rparen),
            kind: ExpressionKind::FunctionCall(FunctionCall {
                callee: Box::new(callee),
                args,
            }),
        }
    }

    fn parse_arg(&mut self) -> Arg {
        Arg {
            label: None,
            value: Box::new(self.parse_expression()),
        }
    }

    fn parse_var_expression(&mut self) -> Expression {
        let name = self.parse_identifier();
        Expression {
            location: name.location(),
            kind: ExpressionKind::Var(name),
        }
    }

    fn expression_predicted(&self) -> bool {
        use t::*;
        match self.current_token().kind {
            Identifier => true,
            BinaryStringLiteral => true,
            LParen => true,
            _ => false,
        }
    }

    fn parse_function_signature(&mut self) -> FunctionSignature {
        let start = self.expect(t::Def);
        let name = self.parse_identifier();
        self.expect(t::LParen);
        let params = self.parse_comma_separated_list(t::RParen, |p| p.parse_param());
        let rparen = self.expect(t::RParen);
        let return_type = self.parse_optional_type_annotation();
        FunctionSignature {
            location: SourceLocation::between_optional(&start, &rparen, &return_type),
            name,
            params,
            return_type,
        }
    }

    fn parse_comma_separated_list<T, F: Fn(&mut Parser) -> T>(
        &mut self,
        terminator: TokenKind,
        parse_one: F,
    ) -> Vec<T> {
        self.parse_separated_list(t::Comma, terminator, parse_one)
    }

    fn parse_separated_list<T, F: Fn(&mut Parser) -> T>(
        &mut self,
        separator: TokenKind,
        terminator: TokenKind,
        parse_one: F,
    ) -> Vec<T> {
        let mut result = vec![];
        let mut first = true;
        while !self.at(terminator) && !self.at(t::End) {
            if !first {
                self.expect(separator);
            }
            first = false;
            result.push(parse_one(self));
        }

        result
    }

    fn parse_param(&mut self) -> Param {
        let name = self.parse_identifier();
        let annotation = self.parse_optional_type_annotation();
        Param { name, annotation }
    }

    fn parse_optional_type_annotation(&mut self) -> Option<TypeAnnotation> {
        if self.at(t::Colon) {
            self.advance();
            Some(self.parse_type_annotation())
        } else {
            None
        }
    }

    fn parse_extern_function_def(&mut self) -> Declaration {
        let start = self.expect(t::Extern);
        self.expect(t::Def);
        let name = self.parse_identifier();
        self.expect(t::LParen);
        let params = self.parse_comma_separated_list(t::RParen, |p| p.parse_type_annotation());
        self.expect(t::RParen);
        self.expect(t::Colon);
        let return_type = self.parse_type_annotation();
        self.expect(t::Equal);

        let extern_name = self.parse_identifier();

        self.expect(t::Semicolon);

        Declaration {
            location: SourceLocation::between(&start, &extern_name),
            kind: DeclarationKind::ExternFunctionDef(ExternFunctionDef {
                name,
                params,
                return_type,
                extern_name,
            }),
        }
    }

    fn parse_type_annotation(&mut self) -> TypeAnnotation {
        match self.current_token().kind {
            t::Identifier => self.parse_var_type_annotation(),
            t::Star => self.parse_pointer_type_annotation(),
            kind => todo!("{:?}", kind),
        }
    }

    fn parse_pointer_type_annotation(&mut self) -> TypeAnnotation {
        let start = self.expect(t::Star);
        let is_mutable = if self.at(t::Mut) {
            self.advance();
            true
        } else {
            false
        };
        let to_type = Box::new(self.parse_type_annotation());
        let location = SourceLocation::between(&start, &to_type);
        TypeAnnotation {
            location,
            kind: if is_mutable {
                TypeAnnotationKind::MutPointer(to_type)
            } else {
                TypeAnnotationKind::Pointer(to_type)
            },
        }
    }

    fn parse_var_type_annotation(&mut self) -> TypeAnnotation {
        TypeAnnotation {
            location: self.current_token().location,
            kind: TypeAnnotationKind::Var(self.parse_identifier()),
        }
    }

    fn parse_import_as_declaration(&mut self) -> Declaration {
        let start = self.expect(t::Import);
        let path = self.parse_qualified_path();
        self.expect(t::As);
        let as_name = self.parse_identifier();
        self.expect(t::Semicolon);
        Declaration {
            location: SourceLocation::between(&start, &as_name),
            kind: DeclarationKind::ImportAs(ImportAs { path, as_name }),
        }
    }

    fn parse_identifier(&mut self) -> Identifier {
        let token = self.expect(t::Identifier);
        Identifier {
            location: token.location,
            name: token.text,
        }
    }

    fn parse_qualified_path(&mut self) -> QualifiedPath {
        let first = self.parse_identifier();
        let mut names = vec![first];
        while self.at(t::Dot) {
            self.advance();
            names.push(self.parse_identifier());
        }
        QualifiedPath(names)
    }

    fn expect(&mut self, kind: TokenKind) -> Token {
        if !self.at(kind) {
            panic!(
                "{:?}: Unexpected token: ({}: {:?})",
                self.current_token().location(),
                self.current_token().text,
                self.current_token().kind,
            );
        }
        self.advance()
    }

    fn advance(&mut self) -> Token {
        std::mem::replace(&mut self.current_token, self.lexer.next_token())
    }

    fn current_token(&self) -> &Token {
        &self.current_token
    }

    fn at(&self, token_kind: TokenKind) -> bool {
        self.current_token().kind == token_kind
    }
}
