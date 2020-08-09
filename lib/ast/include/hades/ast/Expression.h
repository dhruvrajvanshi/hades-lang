//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_AST_EXPRESSION_H
#define HADES_AST_EXPRESSION_H

#include "Identifier.h"
#include "hades/core/location.h"

namespace hades {

class Expression {
public:
  enum class Kind;

private:
  SourceLocation m_location;
  Kind m_kind;

protected:
  Expression(SourceLocation location, Kind kind) noexcept;

public:
  auto location() const -> const SourceLocation &;

  auto kind() const -> Kind { return m_kind; }

  template <typename T> auto as() {
    assert(kind() == T::kind);
    return static_cast<const T *>(this);
  }

  enum class Kind {
    ERROR,
    VAR,
    CALL,
    INT_LITERAL,
  };
};

class IntLiteral : public Expression {
  i64 m_value;

public:
  static constexpr Kind kind = Kind::INT_LITERAL;
  IntLiteral(SourceLocation, i64 value) noexcept;
};

class VarExpression : public Expression {
  Identifier m_name;

public:
  static constexpr Kind kind = Kind::VAR;
  VarExpression(Identifier name) noexcept;
};

class Arg {
  Optional<Identifier> m_label;
  const Expression *m_value;

public:
  Arg(Optional<Identifier> label, const Expression *value) noexcept;
  auto label() const -> const Optional<Identifier> &;
  auto value() const -> const Expression &;
  auto location() const -> SourceLocation;
};

class Call : public Expression {
public:
  using Args = SmallVec<const Arg *, 4>;

private:
  const Expression *m_callee;
  Args m_args;

public:
  static constexpr Kind kind = Kind::CALL;
  Call(SourceLocation location, const Expression *callee, Args &&args) noexcept;

  auto callee() const -> const Expression &;
  auto args() const -> const Args &;
};

} // namespace hades

#endif // HADES_AST_EXPRESSION_H
