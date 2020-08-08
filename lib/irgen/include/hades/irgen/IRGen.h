//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_IRGEN_H
#define HADES_IRGEN_H

#include "hades/ast/SourceFile.h"
#include "hades/context/Context.h"

namespace hades {

class IRGen {
  core::Context* m_ctx;
public:
  IRGen(core::Context* m_ctx) noexcept;

  auto lower_source_file(const SourceFile* source_file) -> void;


};

} // namespace hades

#endif // HADES_IRGEN_H
