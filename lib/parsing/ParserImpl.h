//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_PARSERIMPL_H
#define HADES_PARSERIMPL_H
#include "hades/ast/SourceFile.h"
#include "hades/base.h"
#include "hades/core/Context.h"

namespace hades {

class ParserImpl {
  const fs::path *m_path;
  core::Context *m_ctx;

public:
  ParserImpl(core::Context *m_ctx, const fs::path *m_path);
  ~ParserImpl() = default;
  auto parse_source_file() -> const SourceFile *;

private:
  auto allocator() -> llvm::BumpPtrAllocator &;

  template <typename T, typename ...Args>
  auto allocate(Args&&... args) -> T* {
    auto* mem = allocator().Allocate(sizeof(T), alignof(T));
    return new(mem) T(std::forward<Args>(args)...);
  }
};

} // namespace hades

#endif // HADES_PARSERIMPL_H
