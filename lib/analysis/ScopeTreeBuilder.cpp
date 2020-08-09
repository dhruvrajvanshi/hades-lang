//
// Created by dhruv on 09/08/20.
//

#include "ScopeTreeBuilder.h"

namespace hades {
using t = ScopeTreeBuilder;

ScopeTreeBuilder::ScopeTreeBuilder(BumpPtrAllocator * allocator) noexcept : m_allocator{allocator} {}

auto t::build_scope_tree(const SourceFile &source_file) -> const ScopeTree * {
  auto *root = allocator().allocate<ScopeTree>(
      &source_file, optional::none<const ScopeTree *>());
  auto children = Vec<const ScopeTree *>();

  for (auto &declaration : source_file.declarations()) {
    using k = Declaration::Kind;
    switch (declaration->kind()) {
    case k::STRUCT_DEF:
      break;
    case k::EXTERN_DEF:
      break;
    case k::FUNCTION_DEF: {
      children.push_back(
          build_function_def_scope_tree(declaration->as<FunctionDef>(), root));
      break;
    };
    case k::ERROR:
      break;
    }
  }
  auto children_ref = allocator().copy_items(children);
  root->set_children(children_ref);

  return root;
}

auto t::build_function_def_scope_tree(const FunctionDef &def,
                                      const ScopeTree *parent)
    -> const ScopeTree * {
  auto *tree = allocator().allocate<ScopeTree>(&def, parent);

  auto children = Vec<const ScopeTree *>();

  children.push_back(build_block_scope_tree(def.body(), tree));

  tree->set_children(allocator().copy_items(children));
  return tree;
}

auto t::build_block_scope_tree(const Block & block, const ScopeTree *parent) -> const ScopeTree * {
  auto* tree = allocator().allocate<ScopeTree>(&block, parent);

  return tree;
}

auto t::allocator() -> BumpPtrAllocator & { return *m_allocator; }

} // namespace hades
