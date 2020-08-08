//
// Created by dhruv on 07/08/20.
//

#include "ParserImpl.h"

namespace hades {

using t = ParserImpl;

auto t::parse_expression() -> const Expression * {
  const Expression *head = nullptr;
  switch (auto kind = current_token().kind()) {
#define match(kind, expr)                                                      \
  case kind:                                                                   \
    head = expr;                                                               \
    break

    match(tt::INT, parse_int_literal());
    match(tt::ID, parse_var_expression());
#undef match
  default:
    unimplemented();
  }
  assert(head != nullptr);
  return parse_expression_tail(head);
}

auto ParserImpl::parse_expression_tail(const Expression *head)
    -> const Expression * {
  switch (current_token().kind()) {
  case tt::LPAREN: {
    auto start = advance();
    auto args = Call::Args();

    while (!at(tt::ENDF) && !at(tt::RPAREN)) {
      vec::push_back(args, parse_arg());
      if (!at(tt::RPAREN) && !at(tt::ENDF)) {
        expect(tt::COMMA);
      }
      // parse optional trailing comma
      if (at(tt::COMMA) && peek<1>().is(tt::RPAREN)) {
        advance();
      }
    }

    auto stop = expect(tt::RPAREN);
    auto location = make_location(start, stop);
    return allocate<Call>(location, head, std::move(args));
  }
  default:
    return head;
  }
}

auto t::parse_arg() -> const Arg * {
  if (at(tt::ID) && peek<1>().is(tt::COLON)) {
    auto name = optional::some(parse_identifier());
    expect(tt::COLON);
    return allocate<Arg>(name, parse_expression());
  }
  return allocate<Arg>(optional::none<Identifier>(), parse_expression());
}

auto t::parse_var_expression() -> const Expression * {
  auto identifier = parse_identifier();
  return allocate<VarExpression>(identifier);
}

auto t::parse_int_literal() -> const IntLiteral * {
  auto token = expect(tt::INT);
  auto value = std::stod(String(token.text()));
  return allocate<IntLiteral>(token.location(), value);
}

} // namespace hades
