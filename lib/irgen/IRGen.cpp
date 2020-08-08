//
// Created by dhruv on 08/08/20.
//

#include "hades/irgen/IRGen.h"

namespace hades {

IRGen::IRGen(core::Context *m_ctx) noexcept : m_ctx(m_ctx) {}

auto IRGen::lower_source_file(const SourceFile *source_file) -> void {
  unimplemented();
}

} // namespace hades
