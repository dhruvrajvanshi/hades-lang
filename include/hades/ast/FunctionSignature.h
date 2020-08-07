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
public:
  using Params = SmallVec<const Param*, 4>;
private:
  SourceLocation m_location;
  Identifier m_name;
  Params m_params;
  Optional<const Type*> m_return_type;
public:
  FunctionSignature(SourceLocation location, Identifier name, Params params, Optional<const Type*> return_type) noexcept;

  static auto add_param(Params&, const Param*) -> void;
};

} // namespace hades

#endif // HADES_FUNCTIONSIGNATURE_H
