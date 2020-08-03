//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_AST_ASTNODE_H
#define HADES_AST_ASTNODE_H

#include "hades/core/location.h"

namespace hades {
class ASTNode {
  SourceLocation position();
};
}; // namespace hades

#endif // HADES_ASTNODE_H
