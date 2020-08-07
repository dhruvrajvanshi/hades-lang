//
// Created by dhruv on 07/08/20.
//

#ifndef HADES_BLOCK_H
#define HADES_BLOCK_H

#include "hades/ast/Statement.h"

namespace hades {

class Block {
  Vec<Statement> m_statements;
};

} // namespace hades

#endif // HADES_BLOCK_H
