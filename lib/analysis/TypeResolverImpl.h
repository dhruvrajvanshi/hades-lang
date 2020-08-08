//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_TYPERESOLVERIMPL_H
#define HADES_TYPERESOLVERIMPL_H

#include "hades/analysis/TypeResolutionResult.h"
#include "hades/ast/Type.h"
#include "hades/base.h"
#include "hades/context/Context.h"

namespace hades {

class TypeResolverImpl {
public:
  TypeResolverImpl() noexcept;
  ~TypeResolverImpl() noexcept = default;
  HADES_DEFAULT_MOVE(TypeResolverImpl)

  auto resolve_type_var(const type::Var &) -> TypeResolutionResult;
};

} // namespace hades

#endif // HADES_TYPERESOLVERIMPL_H
