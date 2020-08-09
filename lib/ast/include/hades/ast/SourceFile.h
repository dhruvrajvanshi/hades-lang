//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_AST_SOURCEFILE_H
#define HADES_AST_SOURCEFILE_H

#include "Declaration.h"
#include "hades/base/data.h"
#include "type_traits"

namespace hades {

class SourceFile {
private:
  SourceLocation m_location;
  Vec<const Declaration *> m_declarations;
public:
  SourceFile(SourceLocation, Vec<const Declaration*>);
  auto declarations() const -> ArrayRef<const Declaration*>;
  auto location() const -> const SourceLocation&;
};

static_assert(std::is_move_constructible_v<SourceFile>);

} // namespace hades

#endif // HADES_AST_SOURCEFILE_H
