//
// Created by dhruv on 07/08/20.
//

#include "ParserImpl.h"

namespace hades {

using t = ParserImpl;

auto t::parse_statement() -> const Statement * {
  switch (current_token().kind()) {
  case tt::VAL:
    return parse_val_statement();
  default:
    return parse_expression_statement();
  }
}

auto t::parse_val_statement() -> const ValStatement * {
  auto start = expect(tt::VAL);
  auto name = parse_identifier();
  auto annotation = parse_optional_type_annotation();
  expect(tt::EQ);
  const auto *initializer = parse_expression();
  expect(tt::SEMICOLON);
  return allocate<ValStatement>(name,
                                annotation, initializer);
}

auto t::parse_expression_statement() -> const ExpressionStatement * {
  unimplemented();
}

} // namespace hades
