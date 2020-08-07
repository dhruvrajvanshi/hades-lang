//
// Created by dhruv on 07/08/20.
//

#include "hades/ast/Block.h"

namespace hades {

Block::Block(SourceLocation location, Vec<const Statement*>&& statements) noexcept
    : m_location(location), //
      m_statements(std::move(statements)) {}

auto Block::location() const -> const SourceLocation & {
  return m_location;
}

} // namespace hades
