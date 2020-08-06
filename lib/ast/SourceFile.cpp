#include "hades/ast/SourceFile.h"

namespace hades {

SourceFile::SourceFile(Vec<const Declaration *> declarations)
    : m_declarations(std::move(declarations)) {}

auto SourceFile::declarations() const -> ArrayRef<const Declaration *> {
  return hades::ArrayRef<const Declaration *>(m_declarations);
}

} // namespace hades