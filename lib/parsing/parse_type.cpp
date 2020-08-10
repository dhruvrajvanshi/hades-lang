//
// Created by dhruv on 07/08/20.
//

#include "ParserImpl.h"

namespace hades {
using T = ParserImpl;

auto T::parse_optional_type_annotation() -> Optional<const TypeAnnotation *> {
  if (at(tt::COLON)) {
    advance();
    return { parse_type() };
  }
  return {};
}

auto T::parse_type() -> const TypeAnnotation * {
  auto first = current_token().kind();
  switch (first) {
  case tt::ID: return parse_var_type();
  case tt::STAR: return parse_pointer_type();
  default:
    unimplemented();
  }
}

auto T::parse_var_type() -> const type_annotation::Var * {
  auto name = parse_identifier();
  return allocate<type_annotation::Var>(name);
}

auto T::parse_pointer_type() -> const type_annotation::Pointer * {
  auto start = expect(tt::STAR);
  auto is_mutable = false;
  if (at(tt::MUT)) {
    advance();
    is_mutable = true;
  }
  const auto* pointee = parse_type();
  return allocate<type_annotation::Pointer>(make_location(start, pointee->location()), pointee, is_mutable);
}

} // namespace hades
