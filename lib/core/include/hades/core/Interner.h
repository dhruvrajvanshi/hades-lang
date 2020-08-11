//
// Created by dhruv on 11/08/20.
//

#ifndef HADES_INTERNER_H
#define HADES_INTERNER_H

#include "hades/base.h"
#include "hades/core/InternedString.h"

namespace hades {

class Interner {
  BumpPtrAllocator *m_allocator;
  Map<StringView, InternedString> m_interned_strings{};

public:
  Interner(BumpPtrAllocator *allocator) noexcept : m_allocator(allocator) {}

  HADES_DELETE_MOVE(Interner)
  HADES_DELETE_COPY(Interner)

  auto intern_string(StringView text) -> InternedString;
  auto allocator() -> BumpPtrAllocator &;
};

} // namespace hades

#endif // HADES_INTERNER_H
