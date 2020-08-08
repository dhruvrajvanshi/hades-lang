//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_TYPERESOLVER_H
#define HADES_TYPERESOLVER_H

#include "hades/analysis/TypeResolutionResult.h"
#include "hades/ast/Type.h"
#include "hades/base.h"

namespace hades {
namespace core {
  class Context;
} // namespace core
class TypeResolverImpl;
class TypeResolver {
  UniquePtr<TypeResolverImpl> m_impl;

public:
  TypeResolver() noexcept;
  ~TypeResolver() noexcept;
  HADES_DEFAULT_MOVE(TypeResolver)
  HADES_DELETE_COPY(TypeResolver)
  auto resolve_type_var(const type::Var &) -> TypeResolutionResult;

private:
  auto self() -> TypeResolverImpl&;
};

} // namespace hades

#endif // HADES_TYPERESOLVER_H
