//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_IDENTIFIER_H
#define HADES_IDENTIFIER_H

#include "hades/core/Context.h"
#include "hades/core/location.h"
#include "hades/core/InternedString.h"

namespace hades {

class Identifier {
  SourceLocation m_location;
  InternedString m_name;

public:
  Identifier(SourceLocation location, InternedString view);
};

} // namespace hades

#endif // HADES_IDENTIFIER_H
