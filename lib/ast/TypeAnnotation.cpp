//
// Created by dhruv on 07/08/20.
//

#include "hades/ast/TypeAnnotation.h"

namespace hades {

TypeAnnotation::TypeAnnotation(SourceLocation location, Kind kind) noexcept
    : m_location(std::move(location)), m_kind(kind) {}

auto TypeAnnotation::location() const -> const SourceLocation & { return m_location; }

auto TypeAnnotation::kind() const -> TypeAnnotation::Kind {
  return m_kind;
}

namespace type_annotation {

Var::Var(Identifier ident) noexcept
    : TypeAnnotation(ident.location(), Kind::VAR), m_name(std::move(ident)) {}
auto Var::name() const -> const Identifier & { return m_name; }

Pointer::Pointer(SourceLocation location, const TypeAnnotation *pointee,
                 bool is_mutable) noexcept
    : TypeAnnotation(location, Kind::POINTER), //
      m_pointee(pointee),            //
      m_is_mutable(is_mutable)       //
{}

auto Pointer::pointee() const -> const TypeAnnotation * { return m_pointee; }

auto Pointer::is_mutable() const -> bool { return m_is_mutable; }

} // namespace type

} // namespace hades
