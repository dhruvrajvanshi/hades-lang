//
// Created by dhruv on 03/08/20.
//
#include <hades/core/location.h>

namespace hades {

SourcePosition::SourcePosition(LineNumber line, LineNumber column)
    : m_line(line), m_column(column) {}

LineNumber SourcePosition::line() const { return m_line; }
ColumnNumber SourcePosition::column() const { return m_column; }

bool SourcePosition::operator>(const SourcePosition &that) const {
  if (line() == that.line()) {
    return column() > that.column();
  }
  return line() > that.line();
}

bool SourcePosition::operator==(const SourcePosition &that) const {
  return line() == that.line() && column() == that.column();
}

bool SourcePosition::operator<(const SourcePosition &that) const {
  return !(*this > that) && !(*this == that);
}

bool SourcePosition::operator<=(const SourcePosition & that) const {
  return *this < that || *this == that;
}

bool SourcePosition::operator>=(const SourcePosition &that) const {
  return *this > that || *this == that;
}

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

auto SourceLocation::stop() const noexcept -> SourcePosition { return m_stop; }

auto SourceLocation::path() const noexcept -> const fs::path * {
  return m_path;
}
auto SourceLocation::location() const -> const SourceLocation & {
  return *this;
}
auto SourceLocation::contains(const SourceLocation &that) const -> bool {
  if (path() != that.path()) {
    return false;
  }
  return start() <= that.start() && stop() >= that.stop();
}

} // namespace hades
