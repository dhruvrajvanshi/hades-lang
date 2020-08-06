//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_INTERNEDSTRING_H
#define HADES_INTERNEDSTRING_H

#include "hades/base.h"

namespace hades {

class InternedString {
  const String* m_text_ptr;
public:
  const String& operator*() const noexcept;
  const String* operator->() const noexcept;

  HADES_DEFAULT_MOVE(InternedString)
  HADES_DEFAULT_COPY(InternedString)

  InternedString(const String* text) noexcept;
};

} // namespace hades

#endif // HADES_INTERNEDSTRING_H
