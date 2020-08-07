//
// Created by dhruv on 07/08/20.
//
#include "hades/ast/Expression.h"

namespace hades {

auto Expression::location() const -> const SourceLocation & {
  return m_location;
}

Expression::Expression(SourceLocation location, Kind kind) noexcept
    : m_location{location}, m_kind{kind} {}

IntLiteral::IntLiteral(SourceLocation location, i64 value) noexcept
    : Expression(location, Kind::INT_LITERAL), m_value{value} {}

VarExpression::VarExpression(Identifier name) noexcept
    : Expression(name.location(), Kind::VAR), m_name(name) {}

} // namespace hades