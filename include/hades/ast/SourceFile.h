//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_AST_SOURCEFILE_H
#define HADES_AST_SOURCEFILE_H

#include "hades/ast/Declaration.h"
#include "hades/base/data.h"
#include "type_traits"

namespace hades {

class SourceFile {
private:
  Vec<Declaration *> m_declarations;

public:
  ArrayRef<Declaration *> declarations() {
    return ArrayRef<Declaration*>(m_declarations);
  }
};

} // namespace hades

#endif // HADES_AST_SOURCEFILE_H
