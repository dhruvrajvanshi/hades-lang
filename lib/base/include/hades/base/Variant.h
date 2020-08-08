//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_VARIANT_H
#define HADES_VARIANT_H

#include "variant"

namespace hades {

template <typename... Ts>
class Variant {
  std::variant<Ts...> m_impl;
public:
  template <typename T>
  Variant(T value) noexcept : m_impl{value} {}

  template <typename T>
  auto is() const -> bool {
    return std::get_if<T, Ts...>(&m_impl) != nullptr;
  }

  template <typename T>
  auto as() const -> T& {
    assert(is<T>(m_impl));
    return std::get<T, Ts...>(&m_impl);
  }
};

}

#endif // HADES_VARIANT_H
