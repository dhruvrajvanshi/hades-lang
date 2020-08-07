//
// Created by dhruv on 07/08/20.
//

#include "ParserImpl.h"

namespace hades {

using t = ParserImpl;

auto t::parse_expression() -> const Expression * {
  const Expression *head = nullptr;
  switch (auto kind = current_token().kind()) {
#define match(kind, expr) case kind: head = expr; break;
  match(tt::INT, parse_int_literal())
  match(tt::ID, parse_var_expression())
  default:
    unimplemented();
#undef match
  }
  assert(head != nullptr);
  return head;
}

auto t::parse_var_expression() -> const Expression * {
  auto identifier = parse_identifier();
  return allocate<VarExpression>(identifier);
}

auto t::parse_int_literal() -> const IntLiteral* {
  auto token = expect(tt::INT);
  auto value = std::stod(String(token.text()));
  return allocate<IntLiteral>(token.location(), value);
}

} // namespace hades
