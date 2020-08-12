//
// Created by dhruv on 10/08/20.
//

#include "hades/analysis/Typer.h"

namespace hades {

auto Typer::type_of_val_statement(const ValStatement &) -> const Type * {
  unimplemented();
}

auto Typer::annotation_to_type(const TypeAnnotation & annotation) -> const Type * {
  using k = TypeAnnotation::Kind;
  switch (annotation.kind()) {
  case k::VAR:
    return var_annotation_to_type(annotation.as<type_annotation::Var>());
  case k::POINTER:
    return pointer_annotation_to_type(annotation.as<type_annotation::Pointer>());
  }
}

auto Typer::var_annotation_to_type(const type_annotation::Var & var) -> const Type * {
  unimplemented();
}

auto Typer::pointer_annotation_to_type(const type_annotation::Pointer &) -> const Type * {
  unimplemented();
}

} // namespace hades
