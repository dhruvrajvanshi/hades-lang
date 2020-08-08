//
// Created by dhruv on 07/08/20.
//

#include "hades/ast/Statement.h"
#include "hades/core/location.h"

namespace hades {

Statement::Statement(SourceLocation location, Kind kind) noexcept
    : m_location{location}, m_kind{kind} {};

ValStatement::ValStatement(                 //
    SourceLocation location,
    Identifier name,                        //
    Optional<const Type *> annotation,      //
    const Expression *initializer) noexcept //
    : Statement(location, Kind::VAL),
      m_name{name},             //
      m_annotation{annotation}, //
      m_initializer{initializer} {}

} // namespace hades
