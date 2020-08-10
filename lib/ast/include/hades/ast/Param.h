//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_PARAM_H
#define HADES_PARAM_H

#include "Identifier.h"
#include "TypeAnnotation.h"
#include "hades/base.h"

namespace hades {

class Param {
  SourceLocation m_location;
  Optional<Identifier> m_label;
  Optional<const TypeAnnotation *> m_type_annotation;

public:
  Param(SourceLocation location, Optional<Identifier> label, Optional<const TypeAnnotation *> type) noexcept;

  auto label() const noexcept -> const Optional<Identifier> &;
  auto type_annotation() const noexcept -> const Optional<const TypeAnnotation *> &;
  auto location() const noexcept -> const SourceLocation&;
};

} // namespace hades

#endif // HADES_PARAM_H
