#ifndef HADES_CORE_CONTEXT_H
#define HADES_CORE_CONTEXT_H

#include "hades/base/data.h"
#include "hades/core/CommandLineFlags.h"
#include "hades/core/InternedString.h"
#include "hades/analysis/TypeResolver.h"

namespace hades::core {
class ContextImpl;
class Context {
  UniquePtr<ContextImpl> m_self;

public:
  Context() = delete;
  ~Context() noexcept;
  Context(ContextImpl self);
  HADES_DEFAULT_MOVE(Context)
  HADES_DELETE_COPY(Context)
  static auto from_args(const Vec<String> &) -> Result<Context, FlagParseError>;

  auto run() -> int;
  auto allocator() -> llvm::BumpPtrAllocator &;

  auto intern_string(StringView text) -> InternedString;

  auto type_resolver() -> TypeResolver&;

private:
  auto self_mut() -> ContextImpl &;
  auto self() -> const ContextImpl &;
};

} // namespace hades::core

#endif