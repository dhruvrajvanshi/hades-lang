//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_IDENTIFIER_H
#define HADES_IDENTIFIER_H

#include "hades/context/Context.h"
#include "hades/core/InternedString.h"
#include "hades/core/location.h"

namespace hades {

class Identifier {
  SourceLocation m_location;
  InternedString m_name;

public:
  Identifier(SourceLocation location, InternedString view);

  auto location() const -> const SourceLocation&;
  auto name() const -> InternedString;
};

} // namespace hades

#endif // HADES_IDENTIFIER_H
