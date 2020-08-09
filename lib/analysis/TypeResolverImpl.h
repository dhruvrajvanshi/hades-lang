//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_TYPERESOLVERIMPL_H
#define HADES_TYPERESOLVERIMPL_H

#include "ScopeTree.h"
#include "hades/analysis/TypeResolutionResult.h"
#include "hades/ast/Type.h"
#include "hades/base.h"
#include "hades/base/BumpPtrAllocator.h"
#include "hades/context/Context.h"

namespace hades {

class TypeResolverImpl {
  core::Context* m_ctx;
  BumpPtrAllocator m_allocator{};

#define BUILTIN_INT(name, width, is_signed) \
  const TypeResolutionResult::Int* builtin_##name = m_allocator.allocate<TypeResolutionResult::Int>(width, is_signed); \
  InternedString builtin_name_##name = InternedString(nullptr, 0);

  BUILTIN_INT(u32, 32, false)
  BUILTIN_INT(i32, 32, true)
  BUILTIN_INT(u64, 64, false)
  BUILTIN_INT(i64, 64, true)

#undef BUILTIN_INT
public:
  TypeResolverImpl(core::Context*) noexcept;
  ~TypeResolverImpl() noexcept = default;
  HADES_DEFAULT_MOVE(TypeResolverImpl)

  auto resolve_type_var(const type::Var &) -> TypeResolutionResult;

private:
  auto ctx() -> core::Context&;

  auto allocator() -> BumpPtrAllocator&;

  auto resolve_type_var_in_scope(const type::Var &, const ScopeTree&) const -> TypeResolutionResult;
  auto resolve_type_var_in_source_file(const type::Var &, const SourceFile&) const -> TypeResolutionResult;
};

} // namespace hades

#endif // HADES_TYPERESOLVERIMPL_H
