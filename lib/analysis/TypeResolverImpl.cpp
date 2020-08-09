//
// Created by dhruv on 08/08/20.
//

#include "TypeResolverImpl.h"
#include "ScopeTreeBuilder.h"
#include "hades/ast/Declaration.h"

namespace hades {

TypeResolverImpl::TypeResolverImpl(core::Context *ctx) noexcept : m_ctx{ctx} {}

auto TypeResolverImpl::resolve_type_var(const type::Var &t)
    -> TypeResolutionResult {
  auto &source_file = ctx().get_source_file(*t.name().location().path());
  auto scope_tree_builder = ScopeTreeBuilder{&allocator()};
  const auto *scope_tree = scope_tree_builder.build_scope_tree(source_file);
  const auto* node_scope = scope_tree->narrowest_scope_containing(t);
  auto location = node_scope->location();
  unimplemented();
}

auto TypeResolverImpl::ctx() -> core::Context & { return *m_ctx; }

auto TypeResolverImpl::allocator() -> BumpPtrAllocator & { return m_allocator; }


} // namespace hades
