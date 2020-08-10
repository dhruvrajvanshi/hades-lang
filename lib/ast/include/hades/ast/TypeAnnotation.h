//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_TYPEANNOTATION_H
#define HADES_TYPEANNOTATION_H

#include "Identifier.h"

namespace hades {
template <typename T> constexpr auto is_type() -> bool;
template <typename T> constexpr auto assert_is_type() -> void;
class TypeAnnotation {
public:
  enum class Kind;
private:
  SourceLocation m_location;
  Kind m_kind;

protected:
  TypeAnnotation(SourceLocation, Kind) noexcept;
public:
  HADES_DELETE_COPY(TypeAnnotation)
  HADES_DELETE_MOVE(TypeAnnotation)
  auto location() const -> const SourceLocation &;
  auto kind() const -> Kind;

  enum class Kind {
    VAR,
    POINTER,
    INT,
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
  return std::is_assignable_v<TypeAnnotation, T>
    && std::is_same_v<std::decay<decltype(T::kind)>, TypeAnnotation::Kind>;
}

namespace type_annotation {

class Var : public TypeAnnotation {
  Identifier m_name;

public:
  static constexpr Kind kind = Kind::VAR;
  Var(Identifier) noexcept;

  auto name() const -> const Identifier&;
};

class Pointer : public TypeAnnotation {
  const TypeAnnotation * m_pointee;
  bool m_is_mutable;
public:
  static constexpr Kind kind = Kind::POINTER;
  Pointer(SourceLocation location, const TypeAnnotation * pointee, bool is_mutable) noexcept;

  auto is_mutable() const -> bool;
  auto pointee() const -> const TypeAnnotation *;
};

class Int : public TypeAnnotation {
  bool m_is_signed;
  u8 m_width;

public:
  static constexpr Kind kind = Kind::INT;
  Int(SourceLocation location, bool is_signed, u8 width) noexcept
      : TypeAnnotation(location, Kind::INT),
        m_is_signed{is_signed},
        m_width{width} {};

  auto is_signed() const -> bool { return m_is_signed; }
  auto width() const -> u8 { return m_width; }
};

} // namespace type_annotation

} // namespace hades

#endif // HADES_TYPEANNOTATION_H
