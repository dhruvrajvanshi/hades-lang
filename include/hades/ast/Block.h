//
// Created by dhruv on 07/08/20.
//

#ifndef HADES_BLOCK_H
#define HADES_BLOCK_H

#include "hades/ast/Statement.h"

namespace hades {

class Block {
  SourceLocation m_location;
  Vec<const Statement*> m_statements;
public:
  Block(SourceLocation location, Vec<const Statement*>&&) noexcept;
  auto location() const -> const SourceLocation&;
};

} // namespace hades

#endif // HADES_BLOCK_H
