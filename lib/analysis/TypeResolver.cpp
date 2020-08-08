//
// Created by dhruv on 08/08/20.
//
#include "hades/analysis/TypeResolver.h"
#include "TypeResolverImpl.h"

namespace hades {

TypeResolver::TypeResolver(core::Context* ctx) noexcept : m_impl{new TypeResolverImpl(ctx)} {}
TypeResolver::~TypeResolver() noexcept = default;

auto TypeResolver::self() -> TypeResolverImpl & { return *m_impl.get(); }

auto TypeResolver::resolve_type_var(const type::Var &type_var)
    -> TypeResolutionResult {
  return self().resolve_type_var(type_var);
}


} // namespace hades
