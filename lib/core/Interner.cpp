#include "hades/core/Interner.h"

namespace hades {

auto Interner::intern_string(StringView text) -> InternedString {
  if (m_interned_strings.find(text) == m_interned_strings.cend()) {
    auto *buffer =
        (char *)allocator().allocate(text.length() + 1, alignof(char));
    buffer[text.length()] = '\0';
    memcpy((void *)buffer, (void *)text.begin(), text.length());
    auto view = StringView(buffer, text.length());
    m_interned_strings.insert({view, {buffer, text.length()}});
  }
  const auto &item = m_interned_strings.find(text);
  auto interned_str = item->second;
  return interned_str;
}

auto Interner::allocator() -> BumpPtrAllocator & {
  return *m_allocator;
}

} // namespace hades
