#ifndef HADES_CORE_CONTEXT_H
#define HADES_CORE_CONTEXT_H

#include "hades/base/data.h"
#include "hades/core/CommandLineFlags.h"

namespace hades::core {
class ContextImpl;
class Context {
  UniquePtr<ContextImpl> m_self;

public:
  Context() = delete;
  ~Context() noexcept;
  Context(UniquePtr<ContextImpl> self);
  Context(ContextImpl self);
  HADES_DEFAULT_MOVE(Context)
  HADES_DELETE_COPY(Context)
  static auto from_args(const Vec<String>&) -> Result<Context, FlagParseError>;

  auto run() -> int;

private:
  auto self_mut() -> ContextImpl &;
  auto self() -> const ContextImpl &;

};

} // namespace hades::core

#endif