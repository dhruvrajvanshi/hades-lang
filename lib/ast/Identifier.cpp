//
// Created by dhruv on 06/08/20.
//

#include "hades/ast/Identifier.h"
#include "hades/core/InternedString.h"

namespace hades {

Identifier::Identifier(SourceLocation location, InternedString name)
    : m_location(std::move(location)), m_name(name) {}

auto Identifier::location() const -> const SourceLocation & {
  return m_location;
}
auto Identifier::name() const -> InternedString { return m_name; }

} // namespace hades
