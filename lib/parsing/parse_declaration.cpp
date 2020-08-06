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
  unimplemented();
}

} // namespace hades
