//
// Created by dhruv on 03/08/20.
//
#include <hades/core/location.h>

namespace hades {

SourcePosition::SourcePosition(LineNumber line, LineNumber column)
    : m_line(line), m_column(column) {}

LineNumber SourcePosition::line() { return m_line; }
ColumnNumber SourcePosition::column() { return m_column; }

} // namespace hades
