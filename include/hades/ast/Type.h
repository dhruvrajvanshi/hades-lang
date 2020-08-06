//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_TYPE_H
#define HADES_TYPE_H

#include "hades/ast/Identifier.h"

namespace hades {

class Type {
protected:
  SourceLocation m_location;

public:
  Type(SourceLocation);

  auto location() -> const SourceLocation &;
};

namespace type {

class Var : Type {
  Identifier m_name;

public:
};

} // namespace type

} // namespace hades

#endif // HADES_TYPE_H
