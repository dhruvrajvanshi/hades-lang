//
// Created by dhruv on 08/08/20.
//

#include "IRGenImpl.h"

namespace hades {
using t = IRGenImpl;

auto t::lower_source_file() -> void { unimplemented(); }

IRGenImpl::IRGenImpl(core::Context *ctx) noexcept: m_ctx{ctx} {}


} // namespace hades
