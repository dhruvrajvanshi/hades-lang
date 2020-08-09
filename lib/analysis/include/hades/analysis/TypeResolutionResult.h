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
public:
  class Int;
private:
  Variant<const StructDef *, const Int*, const UnresolvedType *> m_impl;

public:
  template <typename T>
  TypeResolutionResult(const T *value) noexcept : m_impl(value){};

  template <typename T> auto is() const -> bool { return m_impl.is<const T*>(); }

  template <typename T> auto as() const -> const T& { return *m_impl.as<const T*>(); }

public:
  struct Int {
    u8 width;
    bool is_signed;
    Int(u8 width, bool is_signed) noexcept : width(width), is_signed(is_signed) {}
  };
};

} // namespace hades

#endif // HADES_TYPERESOLUTIONRESULT_H
