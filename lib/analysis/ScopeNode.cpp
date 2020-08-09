//
// Created by dhruv on 09/08/20.
//

#include "ScopeNode.h"

namespace hades {

auto ScopeNode::parent() const -> Optional<const ScopeNode *> {
  return m_parent;
}

auto ScopeNode::children() const -> ArrayRef<const ScopeNode *> {
  return {m_children};
}

} // namespace hades