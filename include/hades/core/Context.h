#ifndef HADES_CORE_CONTEXT_H
#define HADES_CORE_CONTEXT_H

#include "hades/data.h"

namespace hades::core {

class Context {
public:
  Context(SmallVec<String, 16> args);
  Context(int argc, const char **argv);
  auto run() -> int;
};

} // namespace hades::core

#endif