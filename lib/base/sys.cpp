//
// Created by dhruv on 06/08/20.
//

#include "hades/base/sys.h"

namespace hades {

Unimplemented::Unimplemented(std::string message) noexcept
    : m_message{std::move(message)} {}

namespace path {

auto replace_extension(const Path &path, String &&new_extension) -> Path {
  auto copy = path;
  copy.replace_extension(std::move(new_extension));
  return copy;
}

} // namespace path

} // namespace hades
