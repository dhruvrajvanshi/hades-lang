//
// Created by dhruv on 07/08/20.
//

#include "hades/ast/Statement.h"
#include "hades/core/location.h"

namespace hades {

Statement::Statement(SourceLocation location, Kind kind) noexcept
    : m_location{location}, m_kind{kind} {};

ValStatement::ValStatement( //
    SourceLocation location,
    Identifier name,                                //
    Optional<const TypeAnnotation *> annotation,              //
    const Expression *initializer) noexcept         //
    : Statement(location, Kind::VAL), m_name{name}, //
      m_annotation{annotation},                     //
      m_initializer{initializer} {}

ExpressionStatement::ExpressionStatement(const Expression *expression) noexcept
    : Statement(expression->location(), Kind::EXPRESSION), m_expression{
                                                               expression} {}

auto ExpressionStatement::expression() const -> const Expression & {
  return *m_expression;
}

} // namespace hades
