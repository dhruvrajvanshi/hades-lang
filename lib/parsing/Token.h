//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_TOKEN_H
#define HADES_TOKEN_H

#include "hades/base/data.h"
#include "hades/core/location.h"

namespace hades {

class Token {
public:
  enum class Kind;

private:
  Kind m_kind;
  SourceLocation m_location;
  StringView m_text;

public:
  Token(Kind kind, SourceLocation m_location, StringView m_text) noexcept;
  auto kind() const noexcept -> Kind;
  auto location() const noexcept -> SourceLocation;
  auto text() const noexcept -> StringView;
  auto is(Kind kind) const noexcept -> bool;
  enum class Kind {
    ERROR,
    ID,
    EXTERN,
    DEF,
    STRUCT,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    EQ,
    EQEQ,

    SEMICOLON,

    ENDF,
  };
};

} // namespace hades

#endif // HADES_TOKEN_H
