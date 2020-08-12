//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_NAMERESOLUTIONRESULT_H
#define HADES_NAMERESOLUTIONRESULT_H

#include "hades/ast/Declaration.h"
#include "hades/hir/Type.h"
#include "hades/base.h"

namespace hades {
class Unresolved {};

static Unresolved unresolved{};

class NameResolutionResult {
public:

private:
  Variant<const StructDef *, const Type *, const Unresolved *,
          const ExternDef *, const ValStatement *, const FunctionDef *>
      m_impl;

public:
  template <typename T>
  NameResolutionResult(const T *value) noexcept : m_impl(value){};

  template <typename T> auto is() const -> bool {
    return m_impl.is<const T *>();
  }

  template <typename T> auto as() const -> const T * {
    return m_impl.as<const T *>();
  }
};

} // namespace hades

#endif // HADES_NAMERESOLUTIONRESULT_H
