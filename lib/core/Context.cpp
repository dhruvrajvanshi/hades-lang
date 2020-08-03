#include "hades/core/Context.h"
#include "hades/data.h"

namespace hades::core {

auto Context::run() -> int { return 1; }

Context::Context(SmallVec<String, 16> args) {}
Context::Context(int argc, const char** args) {}

} // namespace hades::core