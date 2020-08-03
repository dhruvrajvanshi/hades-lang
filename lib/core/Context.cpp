
#include "hades/core/Context.h"
#include "ContextImpl.h"
#include "hades/base/data.h"

namespace hades::core {

Context::Context(Vec<String> args): m_self(std::make_unique<ContextImpl>(std::move(args))) {}

Context::Context(int argc, const char** args) {}

auto Context::run() -> int {
  return self_mut().run();
}

auto Context::self_mut() -> ContextImpl& {
  return *m_self;
}

auto Context::self() -> const ContextImpl& {
  return *m_self;
}
Context::~Context() = default;

} // namespace hades::core