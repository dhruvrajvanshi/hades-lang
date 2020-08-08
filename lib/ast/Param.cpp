//
// Created by dhruv on 07/08/20.
//
#include "hades/ast/Param.h"

namespace hades {

Param::Param(SourceLocation location, Optional<Identifier> label, Optional<const Type *> type) noexcept
    : m_location(location), m_label(label), m_type(type) {}

auto Param::location() const noexcept -> const SourceLocation& { return m_location; }

auto Param::type() const noexcept -> const Optional<const Type *> & {
  return m_type;
}

} // namespace hades
