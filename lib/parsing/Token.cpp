//
// Created by dhruv on 06/08/20.
//
#include "Token.h"
namespace hades {

Token::Token(Kind kind, SourceLocation m_location, StringView m_text) noexcept
    : m_kind(kind), m_location(m_location), m_text(m_text) {}

auto Token::kind() const noexcept -> Token::Kind { return m_kind; }

auto Token::is(Token::Kind kind) const noexcept -> bool {
  return kind == m_kind;
}

auto Token::location() const noexcept -> SourceLocation { return m_location; }

auto Token::text() const noexcept -> StringView { return m_text; }

} // namespace hades
