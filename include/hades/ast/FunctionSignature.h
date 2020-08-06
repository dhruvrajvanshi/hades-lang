//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_FUNCTIONSIGNATURE_H
#define HADES_FUNCTIONSIGNATURE_H

#include "hades/ast/Identifier.h"
#include "hades/ast/Type.h"
#include "hades/ast/Param.h"

namespace hades {

class FunctionSignature {
  Identifier m_name;
  Vec<const Param*> m_params;
  Type m_return_type;
};

} // namespace hades

#endif // HADES_FUNCTIONSIGNATURE_H
