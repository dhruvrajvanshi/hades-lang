//
// Created by dhruv on 10/08/20.
//

#ifndef HADES_TYPER_H
#define HADES_TYPER_H

#include "hades/analysis/NameResolver.h"
#include "hades/ast/Declaration.h"
#include "hades/ast/TypeAnnotation.h"
#include "hades/hir/Type.h"

namespace hades {

class Typer {
  NameResolver *m_resolver;
  BumpPtrAllocator *m_allocator;

public:
  Typer(NameResolver *resolver, BumpPtrAllocator *allocator) noexcept
      : m_resolver{resolver}, m_allocator{allocator} {}

  auto type_of_val_statement(const ValStatement &) -> const Type *;
  auto annotation_to_type(const TypeAnnotation &) -> const Type *;

private:
  auto allocator() -> BumpPtrAllocator &;
  auto var_annotation_to_type(const type_annotation::Var &) -> const Type *;
  auto pointer_annotation_to_type(const type_annotation::Pointer &)
      -> const Type *;

  auto resolver() -> NameResolver & { return *m_resolver; }
};

} // namespace hades

#endif // HADES_TYPER_H
