//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_LEXER_H
#define HADES_LEXER_H
#include "hades/base.h"
#include "Token.h"

namespace hades {

class Lexer {
  using tt = Token::Kind;
  const fs::path *m_path;
  String m_text;
  u64 m_start = 0;
  u64 m_current = 0;
  SourcePosition m_start_pos = {1, 1};
  SourcePosition m_current_pos = {1, 1};
  SourcePosition m_last_pos = {1, 1};


public:
  Lexer(const fs::path *path) noexcept;

  auto next_token() -> Token;

private:
  auto advance() -> char;
  auto skip_whitespace() -> void;
  auto current_char() const -> char;
  auto start_token() -> void;
  auto text() const -> StringView;
  auto make_token(Token::Kind kind) const -> Token;
  auto ident_or_keyword() -> Token;
  auto lexeme() const -> StringView;
  auto numeric_literal() -> Token;
};

} // namespace hades

#endif // HADES_LEXER_H
