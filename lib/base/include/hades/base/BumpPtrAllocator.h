//
// Created by dhruv on 09/08/20.
//

#ifndef HADES_BUMPPTRALLOCATOR_H
#define HADES_BUMPPTRALLOCATOR_H

#include "llvm/Support/Allocator.h"

namespace hades {

class BumpPtrAllocator {
  llvm::BumpPtrAllocator m_impl;
public:

  template <typename T, typename ...Args>
  auto allocate(Args&&... args) -> T* {
    auto* mem = m_impl.Allocate(sizeof(T), alignof(T));
    return new(mem) T(std::forward<Args>(args)...);
  }

  template <typename T1, typename T2>
  auto allocate_array_ref(ArrayRef<T1> input, std::function<T2(T1)> f) -> ArrayRef<T2> {
    T2* begin = m_impl.Allocate<T2>();
    T2* current = begin;
    for (auto& item : input) {
      *current = f(item);
      current++;
    }
    return ArrayRef<T2>(begin, input.size());
  }

  template <typename T>
  auto copy_items(ArrayRef<T> input) -> ArrayRef<T> {
    return allocate_array_ref<T, T>(input, [](auto x) { return x; });
  }

  template <typename T>
  auto copy_items(const Vec<T>& input) -> ArrayRef<T> {
    return allocate_array_ref<T, T>(ArrayRef<T>(input), [](auto x) { return x; });
  }
};

} // namespace hades

#endif // HADES_BUMPPTRALLOCATOR_H
