//
// Created by dhruv on 07/08/20.
//
#include "hades/ast/Param.h"

namespace hades {

Param::Param(SourceLocation location, u8 index, Optional<Identifier> label,
             Optional<const TypeAnnotation *> type) noexcept
    : m_location(location), m_index(index), m_label(label),
      m_type_annotation(type) {}

auto Param::location() const noexcept -> const SourceLocation & {
  return m_location;
}

auto Param::type_annotation() const noexcept
    -> const Optional<const TypeAnnotation *> & {
  return m_type_annotation;
}

} // namespace hades
