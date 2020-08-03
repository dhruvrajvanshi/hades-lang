//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_DECLARATION_H
#define HADES_DECLARATION_H

#include "hades/core/location.h"
#include "type_traits"

namespace hades {
class Declaration {
  SourceLocation m_location;

public:
  SourceLocation location() const noexcept { return m_location; }
};

static_assert(std::is_pod_v<Declaration>);

} // namespace hades

#endif // HADES_DECLARATION_H
