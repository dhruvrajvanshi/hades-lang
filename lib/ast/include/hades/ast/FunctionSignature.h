//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_FUNCTIONSIGNATURE_H
#define HADES_FUNCTIONSIGNATURE_H

#include "Identifier.h"
#include "Param.h"
#include "TypeAnnotation.h"

namespace hades {

class FunctionSignature {
public:
  using Params = SmallVec<const Param*, 4>;
private:
  SourceLocation m_location;
  Identifier m_name;
  Params m_params;
  Optional<const TypeAnnotation *> m_return_type_annotation;
public:
  HADES_DELETE_MOVE(FunctionSignature)
  HADES_DELETE_COPY(FunctionSignature)
  FunctionSignature(SourceLocation location, Identifier name, Params&& params, Optional<const TypeAnnotation *> return_type) noexcept;
  auto location() const -> const SourceLocation&;
  auto name() const -> const Identifier&;
  auto params() const -> ArrayRef<const Param*>;
  auto return_type_annotation() const -> const Optional<const TypeAnnotation *>&;
};

} // namespace hades

#endif // HADES_FUNCTIONSIGNATURE_H
