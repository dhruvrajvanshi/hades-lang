#ifndef HADES_LOCATION_H
#define HADES_LOCATION_H
#include "hades/base/sys.h"
#include <cstdint>
#include <hades/base.h>

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
class SourceLocation;

template <typename T> auto get_location(T t) -> const SourceLocation &;

class SourceLocation {
  const fs::path *m_path;
  SourcePosition m_start;
  SourcePosition m_stop;

public:
  SourceLocation(const fs::path *path, SourcePosition start,
                 SourcePosition stop) noexcept;
  HADES_DEFAULT_COPY(SourceLocation);
  HADES_DEFAULT_MOVE(SourceLocation);
  auto path() const noexcept -> const fs::path *;
  auto start() const noexcept -> SourcePosition;
  auto stop() const noexcept -> SourcePosition;

  static auto between(const fs::path *path, SourceLocation start,
                      SourceLocation stop) noexcept -> SourceLocation;

  template <typename Start, typename Stop>
  static auto between(Start start, Stop stop) noexcept -> SourceLocation {
    return between(                 //
        get_location(start).path(), //
        get_location(start),        //
        get_location(stop));
  }

  auto location() const -> const SourceLocation &;
};
static_assert(std::is_trivially_copyable_v<SourceLocation>);
static_assert(std::is_trivially_move_assignable_v<SourceLocation>);

template <typename T> auto get_location(T t) -> const SourceLocation & {
  if constexpr (std::is_pointer_v<T>) {
    return t->location();
  } else {
    return t.location();
  }
}

} // namespace hades
#endif