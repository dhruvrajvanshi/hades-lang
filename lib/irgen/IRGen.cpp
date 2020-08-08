//
// Created by dhruv on 08/08/20.
//

#include "hades/irgen/IRGen.h"
#include "IRGenImpl.h"

namespace hades {

using t = IRGen;

t::~IRGen() noexcept = default;

t::IRGen(core::Context *ctx) noexcept : m_impl{new IRGenImpl(ctx)} {}

auto IRGen::lower_source_file(const SourceFile *source_file) -> void {
  unimplemented();
}

} // namespace hades
