//
// Created by dhruv on 06/08/20.
//

#include "hades/ast/Declaration.h"

namespace hades {

Declaration::Declaration(SourceLocation location, Kind kind) noexcept
    : m_location(std::move(location)), m_kind(kind) {}

auto Declaration::location() const noexcept -> const SourceLocation & {
  return m_location;
}

auto Declaration::kind() const noexcept -> Kind { return m_kind; }

StructDef::StructDef(SourceLocation location, Identifier name,
                     SmallVec<const StructMember *, 8> members) noexcept
    : Declaration{std::move(location), Kind::STRUCT_DEF}, //
      m_members{std::move(members)},                      //
      m_name{std::move(name)}                             //
{}

auto StructDef::identifier() const noexcept -> const Identifier & {
  return m_name;
}

auto StructDef::members() const noexcept -> ArrayRef<const StructMember *> {
  return m_members;
}

} // namespace hades