//
// Created by dhruv on 09/08/20.
//

#ifndef HADES_SCOPETREEBUILDER_H
#define HADES_SCOPETREEBUILDER_H

#include "ScopeTree.h"
#include "hades/ast/SourceFile.h"
#include "hades/base/BumpPtrAllocator.h"

namespace hades {

class ScopeTreeBuilder {
  BumpPtrAllocator* m_allocator;
  auto allocator() -> BumpPtrAllocator&;
public:
  ScopeTreeBuilder(BumpPtrAllocator*) noexcept;

  auto build_scope_tree(const SourceFile&) -> const ScopeTree *;
  auto build_function_def_scope_tree(const FunctionDef&, const ScopeTree * parent) -> const ScopeTree *;

  auto build_block_scope_tree(const Block&, const ScopeTree * parent) -> const ScopeTree *;

};

} // namespace hades

#endif // HADES_SCOPETREEBUILDER_H
