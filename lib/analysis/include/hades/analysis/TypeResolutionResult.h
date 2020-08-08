//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_TYPERESOLUTIONRESULT_H
#define HADES_TYPERESOLUTIONRESULT_H

#include "hades/ast/Declaration.h"
#include "hades/base.h"

namespace hades {
class UnresolvedType {};

static UnresolvedType unresolved_type{};

class TypeResolutionResult {
  Variant<const StructDef *, const UnresolvedType *> m_impl;

public:
  template <typename T>
  TypeResolutionResult(const T *value) noexcept : m_impl(value){};

  template <typename T> auto is() const -> bool { return m_impl.is<T>(); }

  template <typename T> auto as() const -> const T * { return m_impl.as<T>(); }
};

} // namespace hades

#endif // HADES_TYPERESOLUTIONRESULT_H
