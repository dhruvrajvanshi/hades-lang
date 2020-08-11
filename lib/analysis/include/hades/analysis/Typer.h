//
// Created by dhruv on 10/08/20.
//

#ifndef HADES_TYPER_H
#define HADES_TYPER_H

#include "hades/ast/TypeAnnotation.h"
#include "hades/ast/Declaration.h"
#include "hades/hir/Type.h"

namespace hades {

class Typer {
public:
  auto type_of_val_statement(const ValStatement&) const -> const Type*;
private:
};

}

#endif // HADES_TYPER_H
