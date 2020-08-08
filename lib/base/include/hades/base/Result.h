//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_RESULT_H
#define HADES_RESULT_H

#include "variant"
#include "functional"
#include "cassert"

namespace hades {
template <typename T, typename Error> class Result {
  bool m_is_error;
  std::variant<T, Error> m_data;

public:
  Result(T value) : m_is_error(false), m_data(std::move(value)){};

  Result(Error error) : m_is_error(true), m_data(std::move(error)){};

  ~Result() = default;

  auto is_error() const -> bool { return m_is_error; }
  auto is_ok() const -> bool { return !is_error(); }

  auto copy_value() const -> T {
    static_assert(std::is_copy_constructible_v<T>, "T is not copyable");
    assert(is_ok());
    return std::get<T>(m_data);
  }

  auto copy_error() const -> Error {
    static_assert(std::is_copy_constructible_v<Error>);
    assert(is_error());
    return std::get<Error>(m_data);
  }

  auto get_value() -> T & {
    assert(is_ok());
    return std::get<T>(m_data);
  }

  auto get_error() -> Error & {
    assert(is_error());
    return std::get<Error>(m_data);
  }

  template <typename U>
  auto map(std::function<U(T)> f) -> Result<U, Error> {
    if (is_error()) {
      return Result<U, Error>(get_error());
    } else {
      return Result<U, Error>(f(std::move(get_value())));
    }
  }
};

} // namespace hades

#endif // HADES_RESULT_H
