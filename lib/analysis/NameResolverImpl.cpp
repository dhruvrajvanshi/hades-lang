//
// Created by dhruv on 08/08/20.
//

#include "NameResolverImpl.h"
#include "ScopeTreeBuilder.h"
#include "hades/ast/Declaration.h"

namespace hades {

NameResolverImpl::NameResolverImpl(core::Context *ctx) noexcept : m_ctx{ctx} {}

auto NameResolverImpl::resolve_type_var(const type::Var &t)
    -> NameResolutionResult {
  return resolve_name(t.name(), ResolutionContext::TYPE);
}

auto NameResolverImpl::resolve_name(const Identifier &name,
                                    ResolutionContext resolution_context)
    -> NameResolutionResult {
#define RESOLVE_BUILTIN(n)                                                     \
  if (name.name() == builtin_name_##n) {                                       \
    return builtin_##n;                                                        \
  }
  if (resolution_context == ResolutionContext::TYPE) {
    RESOLVE_BUILTIN(i32)
    RESOLVE_BUILTIN(u32)
    RESOLVE_BUILTIN(i64)
    RESOLVE_BUILTIN(u64)
    RESOLVE_BUILTIN(void)
  }
#undef RESOLVE_BUILTIN

  auto &source_file = ctx().get_source_file(*name.location().path());
  auto scope_tree_builder = ScopeTreeBuilder{&allocator()};
  const auto *scope_tree = scope_tree_builder.build_scope_tree(source_file);
  const auto *node_scope = scope_tree->narrowest_scope_containing(name);
  return resolve_in_scope(name, *node_scope, resolution_context);
}

auto NameResolverImpl::ctx() -> core::Context & { return *m_ctx; }

auto NameResolverImpl::allocator() -> BumpPtrAllocator & { return m_allocator; }

auto NameResolverImpl::resolve_in_scope( //
    const Identifier &var,               //
    const ScopeTree &tree,               //
    ResolutionContext resolution_context //
) const -> NameResolutionResult {
  switch (tree.kind()) {
  case ScopeTree::Kind::SOURCE_FILE:
    return find_in_source_file(var, *tree.as<SourceFile>(), resolution_context);
  default: {
    auto parent = tree.parent();
    assert(parent.hasValue() &&
           "Non source file scope node found without parent");
    return resolve_in_scope(var, *parent.getValue(), resolution_context);
  }
  }
}

auto NameResolverImpl::find_in_source_file(    //
    const Identifier &ident,                   //
    const SourceFile &source_file,             //
    const ResolutionContext resolution_context //
) const -> NameResolutionResult {
  for (auto &decl : source_file.declarations()) {
    if (decl->kind() == Declaration::Kind::STRUCT_DEF) {
      auto &struct_def = decl->as<StructDef>();
      if (struct_def.identifier().name() == ident.name()) {
        return NameResolutionResult(&struct_def);
      }
    }
    if (decl->kind() == Declaration::Kind::EXTERN_DEF) {
      if (resolution_context == ResolutionContext::TYPE) {
        continue;
      }
      auto& extern_def = decl->as<ExternDef>();
      if (extern_def.signature().name().name() == ident.name()) {
        return NameResolutionResult(&extern_def);
      }
    }

    if (decl->kind() == Declaration::Kind::FUNCTION_DEF) {
      if (resolution_context == ResolutionContext::TYPE) {
        continue;
      }
      auto& fn_def = decl->as<FunctionDef>();
      if (fn_def.signature().name().name() == ident.name()) {
        return NameResolutionResult(&fn_def);
      }
    }
  }
  return NameResolutionResult(&unresolved);
}

auto NameResolverImpl::resolve_expr_var(const VarExpression & var)
    -> NameResolutionResult {
  return resolve_name(var.name(), ResolutionContext::VALUE);
}

} // namespace hades
