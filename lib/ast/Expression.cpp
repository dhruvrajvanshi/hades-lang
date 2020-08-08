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

Arg::Arg(Optional<Identifier> label, const Expression *value) noexcept
    : m_label(std::move(label)), m_value(value) {}

auto Arg::value() const -> const Expression & { return *m_value; }

auto Arg::label() const -> const Optional<Identifier> & { return m_label; }

auto Arg::location() const -> SourceLocation {
  if (label().hasValue()) {
    return SourceLocation::between(&label().getValue(), &value());
  } else {
    return value().location();
  }
}

Call::Call(SourceLocation location, const Expression *callee,
           Call::Args &&args) noexcept
    : Expression(location, Kind::CALL), m_callee(callee),
      m_args(std::move(args)) {}

auto Call::args() const -> const Args & { return m_args; }

auto Call::callee() const -> const Expression & { return *m_callee; }

} // namespace hades