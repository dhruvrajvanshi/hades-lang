#ifndef HADES_CORE_CONTEXT_H
#define HADES_CORE_CONTEXT_H

#include "hades/base/data.h"

namespace hades::core {
class ContextImpl;
class Context {
public:
  Context(Vec<String> args);
  Context(int argc, const char **argv);
  Context() = delete;
  ~Context();
  auto run() -> int;
private:
  std::unique_ptr<ContextImpl>m_self;
  auto self_mut() -> ContextImpl&;
  auto self() -> const ContextImpl&;
};

} // namespace hades::core

#endif