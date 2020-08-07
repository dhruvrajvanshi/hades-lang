//
// Created by dhruv on 06/08/20.
//

#include "Lexer.h"
#include "fstream"
#include "iostream"

namespace hades {

Lexer::Lexer(const fs::path *path) noexcept : m_path(path), m_text() {
  std::fstream file;
  file.open(path->c_str());

  file.seekg(0, std::ios::end);
  u64 end = file.tellg();
  u64 size = end;
  file.seekg(0, std::ios::beg);

  m_text.reserve(size + 1);

  m_text.assign((std::istreambuf_iterator<char>(file)),
                std::istreambuf_iterator<char>());

  file.close();
}
namespace {

auto is_ident_starter(char c) -> bool {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

auto is_ident_char(char c) -> bool {
  return is_ident_starter(c) || (c >= '0' && c <= '9');
}

} // namespace
auto Lexer::next_token() -> Token {
  skip_whitespace();
  start_token();
  auto first_char = current_char();
  if (first_char == '\0') {
    return make_token(Token::Kind::ENDF);
  }

  if (is_ident_starter(first_char)) {
    return ident_or_keyword();
  }
#define PUNCTUATION(c, kind)                                                   \
  case c:                                                                      \
    advance();                                                                 \
    return make_token(kind);
#define TWO_CHARS(first, kind, second, kind2)                                  \
  case first: {                                                                \
    advance();                                                                 \
    if (current_char() == second) {                                            \
      advance();                                                               \
      return make_token(kind2);                                                \
    } else {                                                                   \
      return make_token(kind);                                                 \
    }                                                                          \
  }
  switch (first_char) {
    PUNCTUATION('{', tt::LBRACE)
    PUNCTUATION('}', tt::RBRACE)
    PUNCTUATION('(', tt::LPAREN)
    PUNCTUATION(')', tt::RPAREN)
    PUNCTUATION(':', tt::COLON)
    PUNCTUATION(';', tt::SEMICOLON)
    PUNCTUATION('*', tt::STAR)
    PUNCTUATION(',', tt::COMMA)

    TWO_CHARS('=', tt::EQ, '=', tt::EQEQ)
  default:
    unimplemented();
  }

#undef PUNCTUATION
#undef TWO_CHARS

  unimplemented();
}

namespace {
auto is_whitespace(char c) -> bool {
  return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}
} // namespace
auto Lexer::skip_whitespace() -> void {
  while (is_whitespace(current_char())) {
    advance();
  }
}

auto Lexer::current_char() const -> char {
  if (m_current < m_text.size()) {
    return m_text[m_current];
  } else {
    return '\0';
  }
}

auto Lexer::advance() -> char {
  auto last_char = current_char();
  m_current++;
  while (current_char() == '\r') {
    m_current++;
  }
  m_last_pos = {m_current_pos.line(), m_current_pos.column()};
  if (last_char == '\n') {
    m_current_pos = {m_current_pos.line() + 1, 1};
  } else {
    m_current_pos = {m_current_pos.line(), m_current_pos.column() + 1};
  }
  return last_char;
}

auto Lexer::start_token() -> void {
  m_start_pos = m_current_pos;
  m_start = m_current;
}

auto Lexer::make_token(Token::Kind kind) const -> Token {
  return Token(kind, SourceLocation(m_path, m_start_pos, m_last_pos), lexeme());
}

auto Lexer::lexeme() const -> StringView {
  return text().substr(m_start, m_current - m_start);
}

auto Lexer::text() const -> StringView { return m_text; }

namespace {

static Map<String, Token::Kind> m_keyword_kinds = {
    {"struct", Token::Kind::STRUCT}, //
    {"def", Token::Kind::DEF},       //
    {"val", Token::Kind::VAL},       //
    {"mut", Token::Kind::MUT},       //
    {"extern", Token::Kind::EXTERN}, //
};

} // namespace

auto Lexer::ident_or_keyword() -> Token {
  while (is_ident_char(current_char())) {
    advance();
  }
  auto kind = Token::Kind::ID;
  auto text = String(lexeme());

  if (m_keyword_kinds.contains(text)) {
    kind = m_keyword_kinds.at(text);
  }
  return make_token(kind);
}

} // namespace hades
