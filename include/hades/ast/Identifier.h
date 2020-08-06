//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_IDENTIFIER_H
#define HADES_IDENTIFIER_H

#include "hades/core/Context.h"
#include "hades/core/location.h"

namespace hades {

class Identifier {
  friend core::Context;
  StringView m_name;
  SourceLocation m_location;

protected:
  Identifier(SourceLocation location, StringView view);
};

} // namespace hades

#endif // HADES_IDENTIFIER_H
