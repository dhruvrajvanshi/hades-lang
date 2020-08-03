#ifndef HADES_DATA_H
#define HADES_DATA_H

#include "memory"
#include "string"
#include "string_view"
#include "vector"
#include "llvm/ADT/SmallVector.h"

namespace hades {
using String = std::string;
using StringView = std::string_view;
using StringRef = llvm::StringRef;
using Twine = llvm::Twine;

template <typename T, typename Alloc = std::allocator<T>>
using Vec = std::vector<T, Alloc>;

template <typename T, unsigned Size>
using SmallVec = llvm::SmallVector<T, Size>;
} // namespace hades

#endif
