#ifndef HADES_BASE_H
#define HADES_BASE_H

#include "hades/base/sys.h"
#include "hades/base/data.h"

namespace hades {

#define HADES_DEFAULT_COPY(T) T(const T&) = default; \
  auto operator=(const T&) -> T& = default;
#define HADES_DEFAULT_MOVE(T) T(T&&) = default; \
  auto operator=(T&&) -> T& = default;

#define HADES_DELETE_COPY(T) T(const T&) = delete; \
  auto operator=(const T&) -> T& = delete;

template <typename T>
constexpr bool is_copyable_v = std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>;
template <typename T>
constexpr bool is_movable_v = std::is_move_constructible_v<T> && std::is_move_assignable_v<T>;

} // namespace hades


#endif