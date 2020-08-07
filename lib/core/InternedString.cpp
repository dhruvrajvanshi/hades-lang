//
// Created by dhruv on 06/08/20.
//

#include "hades/core/InternedString.h"

namespace hades {

InternedString::InternedString(const char *text, Length length) noexcept
    : m_data{text}, m_length{length} {
  assert(text != nullptr);
}

auto InternedString::as_string_view() const -> StringView {
  return {m_data, m_length};
}

auto InternedString::data() const -> const char * { return m_data; }
auto InternedString::operator==(InternedString other) const -> bool {
  return data() == other.data();
}

} // namespace hades
