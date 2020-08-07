//
// Created by dhruv on 07/08/20.
//

#include "ParserImpl.h"

namespace hades {

using t = ParserImpl;

auto t::parse_expression() -> const Expression * {
  const Expression *head = nullptr;
  switch (current_token().kind()) {
  case tt::INT:
    head = parse_int_literal();
    break;
  default:
    unimplemented();
  }
  assert(head != nullptr);
  return head;
}

auto t::parse_int_literal() -> const IntLiteral* {
  auto token = expect(tt::INT);
  auto value = std::stod(String(token.text()));
  return allocate<IntLiteral>(token.location(), value);
}

} // namespace hades
