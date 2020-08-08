//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_TYPE_H
#define HADES_TYPE_H

#include "Identifier.h"

namespace hades {

class Type {
  SourceLocation m_location;

protected:
  Type(SourceLocation) noexcept;
public:
  auto location() const -> const SourceLocation &;
};

namespace type {

class Var : public Type {
  Identifier m_name;

public:
  Var(Identifier) noexcept;

  auto name() const -> const Identifier&;
};

class Pointer : public Type {
  const Type* m_pointee;
  bool m_is_mutable;
public:
  Pointer(SourceLocation location, const Type* pointee, bool is_mutable) noexcept;

  auto is_mutable() const -> bool;
  auto pointee() const -> const Type*;
};

} // namespace type

} // namespace hades

#endif // HADES_TYPE_H
