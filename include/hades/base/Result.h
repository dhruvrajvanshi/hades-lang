//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_RESULT_H
#define HADES_RESULT_H

#include "variant"

namespace hades {
template <typename T, typename Error> class Result {
public:
  auto is_error() -> bool {
    return m_is_error;
  }
  auto is_ok() -> bool {
    return !is_error();
  }

  auto get_data() -> T {
    assert(is_ok());
    return std::get<T>(m_data);
  }
  auto get_error() -> Error {
    assert(is_error());
    return std::get<Error>(m_data);
  }

  static auto ok(T value) -> Result<T, Error> {
    return Result(false, value);
  }
  static auto error(Error error) -> Result<T, Error> {
    return Result(true, error);
  }

private:
  bool m_is_error;
  std::variant<T, Error> m_data;
  Result(bool is_error, T value) : m_is_error(is_error), m_data(std::move(value)) {};
  Result(bool is_error, Error error) : m_is_error(is_error), m_data(std::move(error)) {};
};

namespace result {
template <typename T, typename Error>
auto ok(T value) -> Result<T, Error> {
  return Result<T, Error>::ok(std::move(value));
}

template <typename T, typename Error>
auto error(Error error) -> Result<T, Error> {
  return Result<T, Error>::error(std::move(error));
}

} // namespace result

} // namespace hades

#endif // HADES_RESULT_H
