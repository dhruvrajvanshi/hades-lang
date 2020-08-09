
#include "hades/context/Context.h"
#include "ContextImpl.h"
#include "hades/base/data.h"

namespace hades::core {

auto Context::run() -> int {
  return self_mut().run();
}

auto Context::self_mut() -> ContextImpl & { return *m_self; }

auto Context::self() -> const ContextImpl & { return *m_self; }

Context::Context(CommandLineFlags flags) : m_self{new ContextImpl(this, flags)} {}

auto Context::allocator() -> llvm::BumpPtrAllocator & {
  return self_mut().allocator();
}

auto Context::intern_string(StringView text) -> InternedString {
  return self_mut().intern_string(text);
}

auto Context::type_resolver() -> TypeResolver & {
  return self_mut().type_resolver();
}
auto Context::get_source_file(const fs::path &path) -> const SourceFile & {
  return self_mut().get_source_file(path);
}

Context::~Context() noexcept = default;

} // namespace hades::core
