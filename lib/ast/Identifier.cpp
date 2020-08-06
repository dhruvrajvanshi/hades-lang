//
// Created by dhruv on 06/08/20.
//

#include "hades/ast/Identifier.h"
namespace hades {

Identifier::Identifier(SourceLocation location, StringView name)
    : m_name(name), m_location(std::move(location)) {}

} // namespace hades
