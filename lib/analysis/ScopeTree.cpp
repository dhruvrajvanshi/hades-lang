//
// Created by dhruv on 09/08/20.
//

#include "ScopeTree.h"

namespace hades {

auto ScopeTree::parent() const -> Optional<const ScopeTree *> {
  return m_parent;
}

auto ScopeTree::children() const -> ArrayRef<const ScopeTree *> {
  return {m_children};
}
auto ScopeTree::set_children(ArrayRef<const ScopeTree *> children) -> void {
  m_children = children;
}

} // namespace hades