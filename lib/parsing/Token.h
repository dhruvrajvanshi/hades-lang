//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_TOKEN_H
#define HADES_TOKEN_H

#include "hades/core/location.h"
#include "hades/base/data.h"

namespace hades {

class Token {
public:
  enum class Kind;
private:
  Kind m_kind;
  SourceLocation m_location;
  StringView m_text;
public:
  Token(Kind kind, SourceLocation m_location, StringView m_text);
  enum class Kind {
    ERROR,
    ID,
    EXTERN,
    DEF,

    LPAREN,
    RPAREN,

    EQ,
    EQEQ,

    SEMICOLON,

    ENDF,
  };
};

} // namespace hades

#endif // HADES_TOKEN_H
