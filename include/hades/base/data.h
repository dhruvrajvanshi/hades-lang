#ifndef HADES_DATA_H
#define HADES_DATA_H

#include "memory"
#include "string"
#include "string_view"
#include "vector"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Allocator.h"

namespace hades {
using String = std::string;
using StringView = std::string_view;
using StringRef = llvm::StringRef;
using Twine = llvm::Twine;

template <typename T, typename Alloc = std::allocator<T>>
using Vec = std::vector<T, Alloc>;

template <typename T, unsigned Size>
using SmallVec = llvm::SmallVector<T, Size>;

template <typename ...Ts>
using PointerUnion = llvm::PointerUnion<Ts...>;

template <typename T>
using ArrayRef = llvm::ArrayRef<T>;

} // namespace hades

#endif
