//
// Created by dhruv on 12/08/20.
//

#ifndef HADES_QUALIFIEDNAME_H
#define HADES_QUALIFIEDNAME_H

#include "InternedString.h"

namespace hades {

struct QualifiedName {
private:
  ArrayRef<InternedString> m_names;
public:
  QualifiedName(ArrayRef<InternedString> names) noexcept : m_names(names) {
    assert(names.size() > 0);
  }

  auto names() const -> ArrayRef<InternedString> {
    return m_names;
  }


};

}

#endif // HADES_QUALIFIEDNAME_H
