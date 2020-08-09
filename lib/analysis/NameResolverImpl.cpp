//
// Created by dhruv on 08/08/20.
//

#include "NameResolverImpl.h"
#include "ScopeTreeBuilder.h"
#include "hades/ast/Declaration.h"

namespace hades {

NameResolverImpl::NameResolverImpl(core::Context *ctx) noexcept : m_ctx{ctx} {}

auto NameResolverImpl::resolve_type_var(const type::Var &t)
    -> TypeResolutionResult {
#define RESOLVE_BUILTIN(n)                                                     \
  if (t.name().name() == builtin_name_##n) {                                   \
    return builtin_##n;                                                        \
  }
  RESOLVE_BUILTIN(i32)
  RESOLVE_BUILTIN(u32)
  RESOLVE_BUILTIN(i64)
  RESOLVE_BUILTIN(u64)
  RESOLVE_BUILTIN(void)
#undef RESOLVE_BUILTIN

  auto &source_file = ctx().get_source_file(*t.name().location().path());
  auto scope_tree_builder = ScopeTreeBuilder{&allocator()};
  const auto *scope_tree = scope_tree_builder.build_scope_tree(source_file);
  const auto *node_scope = scope_tree->narrowest_scope_containing(t);
  return resolve_type_var_in_scope(t, *node_scope);
}

auto NameResolverImpl::ctx() -> core::Context & { return *m_ctx; }

auto NameResolverImpl::allocator() -> BumpPtrAllocator & { return m_allocator; }

auto NameResolverImpl::resolve_type_var_in_scope(const type::Var &var,
                                                 const ScopeTree &tree) const
    -> TypeResolutionResult {
  switch (tree.kind()) {
  case ScopeTree::Kind::SOURCE_FILE:
    return resolve_type_var_in_source_file(var, *tree.as<SourceFile>());
  default: {
    auto parent = tree.parent();
    assert(parent.hasValue() &&
           "Non source file scope node found without parent");
    return resolve_type_var_in_scope(var, *parent.getValue());
  }
  }
}

auto NameResolverImpl::resolve_type_var_in_source_file( //
    const type::Var &var, const SourceFile &source_file) const
    -> TypeResolutionResult {
  for (auto &decl : source_file.declarations()) {
    if (decl->kind() == Declaration::Kind::STRUCT_DEF) {
      auto &struct_def = decl->as<StructDef>();
      if (struct_def.identifier().name() == var.name().name()) {
        return TypeResolutionResult(&struct_def);
      }
    }
  }
  return TypeResolutionResult(&unresolved_type);
}

} // namespace hades
