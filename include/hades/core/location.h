#ifndef HADES_LOCATION_H
#define HADES_LOCATION_H
#include "hades/base/sys.h"
#include <cstdint>

namespace hades {

using LineNumber = uint64_t;
using ColumnNumber = uint64_t;

struct SourcePosition {
  LineNumber line();
  ColumnNumber column();
};
static_assert(std::is_pod_v<SourcePosition>);

struct SourceLocation {
  const fs::path* path();
  SourcePosition start();
  SourcePosition stop();
};
static_assert(std::is_pod_v<SourceLocation>);

} // namespace hades
#endif