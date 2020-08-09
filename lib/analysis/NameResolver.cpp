//
// Created by dhruv on 08/08/20.
//
#include "hades/analysis/NameResolver.h"
#include "NameResolverImpl.h"

namespace hades {

NameResolver::NameResolver(core::Context *ctx) noexcept
    : m_impl{new NameResolverImpl(ctx)} {}
NameResolver::~NameResolver() noexcept = default;

auto NameResolver::self() -> NameResolverImpl & { return *m_impl.get(); }

auto NameResolver::resolve_type_var(const type::Var &type_var)
    -> NameResolutionResult {
  return self().resolve_type_var(type_var);
}

auto NameResolver::resolve_expr_var(const VarExpression &var)
    -> NameResolutionResult {
  return self().resolve_expr_var(var);
}

} // namespace hades
