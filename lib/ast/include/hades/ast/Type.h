//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_TYPE_H
#define HADES_TYPE_H

#include "Identifier.h"

namespace hades {

class Type {
public:
  enum class Kind;
private:
  SourceLocation m_location;
  Kind m_kind;

protected:
  Type(SourceLocation, Kind) noexcept;
public:
  auto location() const -> const SourceLocation &;
  enum class Kind {
    VAR,
    POINTER,
  };
};

namespace type {

class Var : public Type {
  Identifier m_name;

public:
  static constexpr Kind kind = Kind::VAR;
  Var(Identifier) noexcept;

  auto name() const -> const Identifier&;
};

class Pointer : public Type {
  const Type* m_pointee;
  bool m_is_mutable;
public:
  static constexpr Kind kind = Kind::POINTER;
  Pointer(SourceLocation location, const Type* pointee, bool is_mutable) noexcept;

  auto is_mutable() const -> bool;
  auto pointee() const -> const Type*;
};

} // namespace type

} // namespace hades

#endif // HADES_TYPE_H
