#ifndef HADES_LOCATION_H
#define HADES_LOCATION_H
#include "hades/base/sys.h"
#include <cstdint>

namespace hades {

using LineNumber = uint64_t;
using ColumnNumber = uint64_t;

struct SourcePosition {
private:
  LineNumber m_line;
  LineNumber m_column;
public:
  SourcePosition(LineNumber line, LineNumber column);
  LineNumber line();
  ColumnNumber column();
};

struct SourceLocation {
  const fs::path* path();
  SourcePosition start();
  SourcePosition stop();
};
static_assert(std::is_pod_v<SourceLocation>);

} // namespace hades
#endif