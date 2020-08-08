//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_INTERNEDSTRING_H
#define HADES_INTERNEDSTRING_H

#include "hades/base.h"
#include "hashtable.h"

namespace hades {

class InternedString {
public:
  using Length = String::size_type;

private:
  const char *m_data;
  Length m_length;

public:
  HADES_DEFAULT_MOVE(InternedString)
  HADES_DEFAULT_COPY(InternedString)

  InternedString(const char *text, Length length) noexcept;

  auto data() const -> const char *;
  auto as_string_view() const -> StringView;

  auto operator==(InternedString other) const -> bool;
};

} // namespace hades

namespace std {

template <> struct hash<hades::InternedString> {
  size_t operator()(const hades::InternedString &s) const noexcept {
    return std::hash<hades::StringView>{}(s.as_string_view());
  }
};

} // namespace std

#endif // HADES_INTERNEDSTRING_H
