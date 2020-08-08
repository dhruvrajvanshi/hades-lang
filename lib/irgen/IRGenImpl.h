//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_IRGENIMPL_H
#define HADES_IRGENIMPL_H

#include "hades/base.h"
#include "hades/context/Context.h"

namespace hades {

class IRGenImpl {
  core::Context* m_ctx;
public:
  IRGenImpl(core::Context* ctx) noexcept;
  ~IRGenImpl() noexcept = default;

  auto lower_source_file() -> void;
};

} // namespace hades

#endif // HADES_IRGENIMPL_H
