//
// Created by dhruv on 07/08/20.
//

#include "hades/ast/Type.h"

namespace hades {

Type::Type(SourceLocation location, Kind kind) noexcept
    : m_location(std::move(location)), m_kind(kind) {}

auto Type::location() const -> const SourceLocation & { return m_location; }

auto Type::kind() const -> Type::Kind {
  return m_kind;
}

namespace type {

Var::Var(Identifier ident) noexcept
    : Type(ident.location(), Kind::VAR), m_name(std::move(ident)) {}

Pointer::Pointer(SourceLocation location, const Type *pointee,
                 bool is_mutable) noexcept
    : Type(location, Kind::POINTER), //
      m_pointee(pointee),            //
      m_is_mutable(is_mutable)       //
{}

auto Pointer::pointee() const -> const Type * { return m_pointee; }

auto Pointer::is_mutable() const -> bool { return m_is_mutable; }

} // namespace type

} // namespace hades
