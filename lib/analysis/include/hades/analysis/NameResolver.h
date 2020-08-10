//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_NAMERESOLVER_H
#define HADES_NAMERESOLVER_H

#include "hades/analysis/NameResolutionResult.h"
#include "hades/ast/TypeAnnotation.h"
#include "hades/base.h"

namespace hades {
namespace core {
  class Context;
} // namespace core
class NameResolverImpl;
class NameResolver {
  UniquePtr<NameResolverImpl> m_impl;

public:
  NameResolver() = delete;
  NameResolver(core::Context*) noexcept;
  ~NameResolver() noexcept;
  HADES_DEFAULT_MOVE(NameResolver)
  HADES_DELETE_COPY(NameResolver)
  auto resolve_type_var(const type_annotation::Var &) -> NameResolutionResult;
  auto resolve_expr_var(const VarExpression&) -> NameResolutionResult;

private:
  auto self() -> NameResolverImpl &;
};

} // namespace hades

#endif // HADES_NAMERESOLVER_H
