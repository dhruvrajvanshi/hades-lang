//
// Created by dhruv on 06/08/20.
//
#include "ParserImpl.h"

namespace hades {

auto ParserImpl::parse_declaration() -> const Declaration * {
  auto start_token = current_token();
  switch (start_token.kind()) {
  case tt::DEF:
    unimplemented();
  case tt::STRUCT:
    return parse_struct_def();
  case tt::EXTERN:
    unimplemented();
  default:
    unimplemented();
  }
}

auto ParserImpl::parse_struct_def() -> const StructDef * {
  auto start = expect(tt::STRUCT);
  auto name = parse_identifier();
  expect(tt::LBRACE);
  auto members = StructDef::Members();
  while (!at(tt::ENDF) && !at(tt::RBRACE)) {
    members.append({ parse_struct_member() });
  }
  auto stop = expect(tt::RBRACE);
  auto location = make_location(start, stop);
  return allocate<StructDef>(location, name, std::move(members));
}

auto ParserImpl::parse_struct_field() -> const StructField * {
  auto start = expect(tt::VAL);
  auto name = parse_identifier();
  auto type = parse_optional_type_annotation();
  auto end_tok = expect(tt::SEMICOLON);
  SourceLocation end = end_tok.location();
  if (type.hasValue()) {
    const auto* t = type.getValue();
    end = t->location();
  }
  return allocate<StructField>(make_location(start, end), name, type);
}

auto ParserImpl::parse_struct_member() -> const StructMember * {
  return parse_struct_field();
}

} // namespace hades
