//
// Created by dhruv on 10/08/20.
//

#include "hades/analysis/Typer.h"

namespace hades {

auto Typer::type_of_val_statement(const ValStatement & statement) -> const Type * {
  if (statement.type_annotation().hasValue()) {
    return annotation_to_type(*statement.type_annotation().getValue());
  }
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
  llvm_unreachable("");
}

auto Typer::var_annotation_to_type(const type_annotation::Var & var) -> const Type * {
  auto resolved = resolver().resolve_type_var(var.name());
  if (resolved.is<Type>()) {
    return resolved.as<Type>();
  }
  if (resolved.is<StructDef>()) {
    auto name = resolver().qualified_struct_name(resolved.as<StructDef>());
    return allocator().allocate<StructRefType>(name, resolved.as<StructDef>());
  }
  unimplemented();
}
auto Typer::allocator() -> BumpPtrAllocator & {
  return *m_allocator;
}

auto Typer::pointer_annotation_to_type(const type_annotation::Pointer & annotation) -> const Type * {
  return allocator().allocate<PointerType>(annotation_to_type(*annotation.pointee()));
}

} // namespace hades
