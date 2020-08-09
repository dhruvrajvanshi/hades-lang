#include "hades/ast/SourceFile.h"

namespace hades {

SourceFile::SourceFile(SourceLocation location, Vec<const Declaration *> declarations)
    : m_location(location), m_declarations(std::move(declarations)) {}

auto SourceFile::declarations() const -> ArrayRef<const Declaration *> {
  return hades::ArrayRef<const Declaration *>(m_declarations);
}

auto SourceFile::location() const -> const SourceLocation & {
  return m_location;
}

} // namespace hades