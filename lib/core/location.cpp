//
// Created by dhruv on 03/08/20.
//
#include <hades/core/location.h>

namespace hades {

SourcePosition::SourcePosition(LineNumber line, LineNumber column)
    : m_line(line), m_column(column) {}

LineNumber SourcePosition::line() { return m_line; }
ColumnNumber SourcePosition::column() { return m_column; }

SourceLocation::SourceLocation(const fs::path *path, SourcePosition start,
                               SourcePosition stop) noexcept
    : m_path{path}, m_start{start}, m_stop{stop} {}

auto SourceLocation::between(const fs::path *path, SourceLocation start,
                             SourceLocation stop) noexcept -> SourceLocation {
  return {path, start.start(), stop.stop()};
}
auto SourceLocation::start() const noexcept -> SourcePosition {
  return m_start;
}

auto SourceLocation::stop() const noexcept  -> SourcePosition {
  return m_stop;
}

auto SourceLocation::path() const noexcept -> const fs::path* {
  return m_path;
}
auto SourceLocation::location() const -> const SourceLocation & {
  return *this;
}

} // namespace hades
