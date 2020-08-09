#ifndef HADES_DATA_H
#define HADES_DATA_H

#include "Result.h"
#include "memory"
#include "string"
#include "string_view"
#include "unordered_set"
#include "vector"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "Variant.h"

namespace hades {
using String = std::string;
using StringView = std::string_view;
using StringRef = llvm::StringRef;
using Twine = llvm::Twine;

template <typename T, typename Alloc = std::allocator<T>>
using Vec = std::vector<T, Alloc>;

template <typename T, unsigned Size>
using SmallVec = llvm::SmallVector<T, Size>;

template <typename... Ts> using PointerUnion = llvm::PointerUnion<Ts...>;

template <typename T> using Optional = llvm::Optional<T>;

template <typename K, typename V> using DenseMap = llvm::DenseMap<K, V>;

template <typename K, typename V> using Map = std::unordered_map<K, V>;

template <typename T> using Set = std::unordered_set<T>;

namespace optional {
template <typename T> auto none() -> Optional<T> { return Optional<T>(); }

template <typename T> auto some(T value) -> Optional<T> {
  return Optional<T>(value);
}
} // namespace optional

template <typename T> using ArrayRef = llvm::ArrayRef<T>;

template <typename... Elements> using Tuple = std::tuple<Elements...>;

template <typename T> using UniquePtr = std::unique_ptr<T>;

using u32 = uint32_t;
using u64 = uint64_t;
using i32 = uint32_t;
using i64 = uint64_t;

template <typename T>
struct IsSmallVector : std::false_type {};

template <typename T, int Size>
struct IsSmallVector<SmallVec<T, Size>> : std::true_type {};


namespace vec {

template <typename Vec, typename T>
auto push_back(Vec& v, T value) -> void {
  if constexpr (IsSmallVector<std::decay<Vec>>::value) {
    v.append({value});
  } else {
    v.push_back(value);
  }
}





} // namespace vec

} // namespace hades

#endif
