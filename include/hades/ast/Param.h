//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_PARAM_H
#define HADES_PARAM_H

#include "hades/ast/Identifier.h"
#include "hades/ast/Type.h"
#include "hades/base.h"

namespace hades {

class Param {
  SourceLocation m_location;
  Optional<Identifier> m_label;
  Optional<const Type *> m_type;

public:
  Param(SourceLocation location, Optional<Identifier> label, Optional<const Type *> type) noexcept;

  auto label() const noexcept -> const Optional<Identifier> &;
  auto type() const noexcept -> const Optional<const Type *> &;
  auto location() const noexcept -> const SourceLocation&;
};

} // namespace hades

#endif // HADES_PARAM_H
