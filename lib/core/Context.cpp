
#include "hades/core/Context.h"
#include "ContextImpl.h"
#include "hades/base/data.h"

namespace hades::core {

auto Context::run() -> int {
  self_mut().set_ctx_ptr(this);
  return self_mut().run();
}

auto Context::self_mut() -> ContextImpl & { return *m_self; }

auto Context::self() -> const ContextImpl & { return *m_self; }
auto Context::from_args(const Vec<String> &args)
    -> Result<Context, FlagParseError> {
  return ContextImpl::from_args(args).map<Context>(
      [](auto &&impl) -> Context { return Context(std::move(impl)); });
}

Context::Context(ContextImpl self) : m_self{new ContextImpl(std::move(self))} {}

auto Context::allocator() -> llvm::BumpPtrAllocator & {
  return self_mut().allocator();
}

auto Context::intern_string(StringView text) -> InternedString {
  return self_mut().intern_string(text);
}

Context::~Context() noexcept = default;

} // namespace hades::core
