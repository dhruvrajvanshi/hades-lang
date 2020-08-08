//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_IRGEN_H
#define HADES_IRGEN_H

#include "hades/ast/SourceFile.h"
#include "hades/context/Context.h"
#include "llvm/IR/LLVMContext.h"

namespace hades {

class IRGenImpl;
class IRGen {
  UniquePtr<IRGenImpl> m_impl;
public:
  IRGen(core::Context* ctx) noexcept;
  ~IRGen() noexcept;

  auto lower_source_file(const SourceFile* source_file) -> void;
};

} // namespace hades

#endif // HADES_IRGEN_H
