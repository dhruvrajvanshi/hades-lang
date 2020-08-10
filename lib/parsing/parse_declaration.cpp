//
// Created by dhruv on 06/08/20.
//
#include "ParserImpl.h"

namespace hades {

auto ParserImpl::parse_declaration() -> const Declaration * {
  auto start_token = current_token();
  switch (start_token.kind()) {
  case tt::DEF:
    return parse_function_def();
  case tt::STRUCT:
    return parse_struct_def();
  case tt::EXTERN:
    return parse_extern_def();
  default:
    unimplemented();
  }
}

auto ParserImpl::parse_function_def() -> const FunctionDef * {
  const auto* signature = parse_function_signature();
  const auto* block = parse_block();
  return allocate<FunctionDef>(signature, block);
}

auto ParserImpl::parse_extern_def() -> const ExternDef * {
  auto start = expect(tt::EXTERN);
  const auto *signature = parse_function_signature();
  expect(tt::EQ);
  auto extern_name = parse_identifier();
  expect(tt::SEMICOLON);
  return allocate<ExternDef>(make_location(start, extern_name), signature,
                             std::move(extern_name));
}

auto ParserImpl::parse_struct_def() -> const StructDef * {
  auto start = expect(tt::STRUCT);
  auto name = parse_identifier();
  expect(tt::LBRACE);
  auto members = StructDef::Members();
  while (!at(tt::ENDF) && !at(tt::RBRACE)) {
    members.append({parse_struct_member()});
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
    const auto *t = type.getValue();
    end = t->location();
  }
  return allocate<StructField>(make_location(start, end), name, type);
}

auto ParserImpl::parse_struct_member() -> const StructMember * {
  return parse_struct_field();
}

auto ParserImpl::parse_function_signature() -> const FunctionSignature * {
  auto start = expect(tt::DEF);
  auto name = parse_identifier();
  expect(tt::LPAREN);
  auto params = FunctionSignature::Params();
  while (!at(tt::RPAREN) && !at(tt::ENDF)) {
    vec::push_back(params, parse_function_signature_param());
    if (!at(tt::RPAREN)) {
      expect(tt::COMMA);
    }
  }
  auto rparen = expect(tt::RPAREN);
  auto return_type = parse_optional_type_annotation();
  auto location = make_location(start, return_type.hasValue()
                                           ? return_type.getValue()->location()
                                           : rparen.location());
  return allocate<FunctionSignature>(location, name, std::move(params), return_type);
}

auto ParserImpl::parse_function_signature_param() -> const Param * {
  if (at(tt::ID) && peek<1>().is(tt::COLON)) {
    auto name = parse_identifier();
    expect(tt::COLON);
    const auto *annotation = parse_type();
    auto name_opt = Optional<Identifier>(name);
    auto location = make_location(name, annotation->location());
    return allocate<Param>(                //
        location,                          //
        name_opt,                          //
        Optional<const TypeAnnotation *>(annotation) //
    );
  }
  auto name = optional::none<Identifier>();
  const auto *annotation = parse_type();
  auto location = make_location(annotation->location(), annotation->location());
  return allocate<Param>( //
      location,           //
      name,               //
      Optional<const TypeAnnotation *>(annotation));
}

} // namespace hades
