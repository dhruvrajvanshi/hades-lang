//
// Created by dhruv on 09/08/20.
//

#ifndef HADES_SCOPENODE_H
#define HADES_SCOPENODE_H

#include "hades/base.h"
#include "hades/ast/SourceFile.h"
#include "hades/ast/Declaration.h"

namespace hades {

class ScopeNode {
  Variant<const SourceFile*, const FunctionDef*, const Block*> m_node;
  Optional<const ScopeNode*> m_parent;
  Vec<const ScopeNode*> m_children;
public:
  template <typename T>
  ScopeNode(Optional<const ScopeNode*> m_parent, const T* node) noexcept : m_node{node} {};

  auto location() const -> const SourceLocation& {
#define CASE(T) if (is<T>()) return as<T>()->location();
    CASE(SourceFile)
    CASE(FunctionDef)
    CASE(Block)
#undef CASE
    llvm_unreachable("");
  }

  template <typename T>
  auto is() const -> bool {
    return m_node.is<const T*>();
  }

  template <typename T>
  auto as() const -> const T* {
    assert(is<T>());
    return m_node.as<const T*>();
  }

  auto parent() const -> Optional<const ScopeNode*>;

  auto children() const -> ArrayRef<const ScopeNode*>;
};

} // namespace hades

#endif // HADES_SCOPENODE_H
