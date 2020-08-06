//
// Created by dhruv on 06/08/20.
//

#include "hades/core/InternedString.h"

namespace hades {

InternedString::InternedString(const String *text_ptr) noexcept
    : m_text_ptr(text_ptr) {}

const String &InternedString::operator*() const noexcept { return *m_text_ptr; }

const String *InternedString::operator->() const noexcept { return m_text_ptr; }

} // namespace hades
