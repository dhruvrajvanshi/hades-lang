//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_TYPE_H
#define HADES_TYPE_H

#include "Identifier.h"

namespace hades {
template <typename T> constexpr auto is_type() -> bool;
template <typename T> constexpr auto assert_is_type() -> void;
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
  auto kind() const -> Kind;

  enum class Kind {
    VAR,
    POINTER,
  };

  template <typename T>
  auto is() const -> bool {
    return kind() == T::kind;
  }

  template <typename T>
  auto as() const -> const T& {
    assert(T::kind == kind());
    return static_cast<const T&>(*this);
  }
};

template <typename T> constexpr auto is_type() -> bool {
  return std::is_assignable_v<Type, T>
    && std::is_same_v<std::decay<decltype(T::kind)>, Type::Kind>;
}

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
