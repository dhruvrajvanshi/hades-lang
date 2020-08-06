//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_PARAM_H
#define HADES_PARAM_H

#include "hades/base.h"
#include "hades/ast/Identifier.h"

namespace hades {

class Param {
  Optional<const Identifier*> m_label;
  Optional<const Type*> m_type;
};

} // namespace hades

#endif // HADES_PARAM_H
