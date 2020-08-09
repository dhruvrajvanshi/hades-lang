//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_NAMERESOLUTIONRESULT_H
#define HADES_NAMERESOLUTIONRESULT_H

#include "hades/ast/Declaration.h"
#include "hades/base.h"

namespace hades {
class Unresolved {};

static Unresolved unresolved{};

class NameResolutionResult {
public:
  class Int;
  class Void;
private:
  Variant<const StructDef *, const Int*, const Void*, const Unresolved *> m_impl;

public:
  template <typename T>
  NameResolutionResult(const T *value) noexcept : m_impl(value){};

  template <typename T> auto is() const -> bool { return m_impl.is<const T*>(); }

  template <typename T> auto as() const -> const T& { return *m_impl.as<const T*>(); }

public:
  struct Int {
    u8 width;
    bool is_signed;
    Int(u8 width, bool is_signed) noexcept : width(width), is_signed(is_signed) {}
  };

  struct Void {};
};

} // namespace hades

#endif // HADES_NAMERESOLUTIONRESULT_H
