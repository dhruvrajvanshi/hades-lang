//
// Created by dhruv on 09/08/20.
//

#ifndef HADES_SCOPETREE_H
#define HADES_SCOPETREE_H

#include "hades/base.h"
#include "hades/ast/SourceFile.h"
#include "hades/ast/Declaration.h"

namespace hades {

class ScopeTree {
public:
  enum class Kind;
private:
  Variant<const SourceFile*, const FunctionDef*, const Block*> m_node;
  Optional<const ScopeTree *> m_parent;
  ArrayRef<const ScopeTree *> m_children;
public:
  enum class Kind {
    SOURCE_FILE,
    FUNCTION_DEF,
    BLOCK,
  };

  template <typename T>
  ScopeTree(const T* node, Optional<const ScopeTree *> parent) noexcept : m_node{node}, m_parent(parent) {};

  template <typename T>
  ScopeTree(const T* node, const ScopeTree * parent) noexcept : m_node{node}, m_parent(parent) {};

  auto kind() const -> Kind {
    if (is<SourceFile>()) return Kind::SOURCE_FILE;
    if (is<FunctionDef>()) return Kind::FUNCTION_DEF;
    if (is<Block>()) return Kind::BLOCK;
    llvm_unreachable("ScopeTree::Kind not defined");
  }

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

  auto parent() const -> Optional<const ScopeTree *>;

  auto children() const -> ArrayRef<const ScopeTree *>;

  auto set_children(ArrayRef<const ScopeTree *> children) -> void;

  template <typename T>
  auto narrowest_scope_containing(const T& node) const -> const ScopeTree* {
    auto node_location = node.location();
    auto scope_location = location();
    assert(scope_location.contains(node_location));
    for (auto* child : children()) {
      auto child_location = child->location();
      if (child_location.contains(node_location)) {
        return child->narrowest_scope_containing(node);
      }
    }
    return this;
  }
};

} // namespace hades

#endif // HADES_SCOPETREE_H
