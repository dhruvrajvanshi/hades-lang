//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_AST_EXPRESSION_H
#define HADES_AST_EXPRESSION_H

#include "hades/ast/Identifier.h"
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
};

class VarExpression : public Expression {
  Identifier m_name;
};

class Arg : public Expression {
  Optional<Identifier> m_label;
  const Expression *m_value;
};

class Call : public Expression {
public:
  using Args = SmallVec<const Arg *, 4>;

private:
  const Expression *m_callee;
  Args m_args;

public:
};

} // namespace hades

#endif // HADES_AST_EXPRESSION_H
