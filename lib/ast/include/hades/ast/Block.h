//
// Created by dhruv on 07/08/20.
//

#ifndef HADES_BLOCK_H
#define HADES_BLOCK_H

#include "Statement.h"

namespace hades {

class Block {
  SourceLocation m_location;
  Vec<const Statement *> m_statements;

public:
  HADES_DELETE_COPY(Block)
  HADES_DELETE_MOVE(Block)
  Block(SourceLocation location, Vec<const Statement *> &&) noexcept;
  auto location() const -> const SourceLocation &;

  auto statements() const -> ArrayRef<const Statement *> {
    return ArrayRef<const Statement *>(m_statements);
  }
};

} // namespace hades

#endif // HADES_BLOCK_H
