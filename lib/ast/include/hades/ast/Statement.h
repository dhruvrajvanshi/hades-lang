//
// Created by dhruv on 07/08/20.
//

#ifndef HADES_STATEMENT_H
#define HADES_STATEMENT_H

#include "Expression.h"
#include "Type.h"
#include "hades/core/location.h"

namespace hades {

class Statement {
public:
  enum class Kind;

private:
  SourceLocation m_location;
  Kind m_kind;

protected:
  Statement(SourceLocation location, Kind kind) noexcept;

public:
  auto location() const -> const SourceLocation & { return m_location; }
  auto kind() const -> Kind { return m_kind; }

  template <typename T> auto as() const -> const T * {
    assert(kind() == T::kind);
    return static_cast<const T *>(this);
  }

  enum class Kind {
    ERROR,

    EXPRESSION,
    VAL,
  };
};

class ValStatement : public Statement {
  const Identifier m_name;
  const Optional<const Type *> m_annotation;
  const Expression *m_initializer;

public:
  static constexpr Kind kind = Kind::VAL;
  ValStatement(SourceLocation, Identifier name,
               Optional<const Type *> annotation,
               const Expression *initializer) noexcept;
};

class ExpressionStatement : public Statement {
  const Expression *m_expression;

public:
  static constexpr Kind kind = Kind::EXPRESSION;
  ExpressionStatement(const Expression *expression) noexcept;
  auto expression() const -> const Expression &;
};

} // namespace hades

#endif // HADES_STATEMENT_H
