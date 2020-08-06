//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_DECLARATION_H
#define HADES_DECLARATION_H

#include "hades/core/location.h"
#include "hades/ast/Identifier.h"
#include "hades/ast/FunctionSignature.h"

namespace hades {
class Declaration {
  SourceLocation m_location;

public:
  SourceLocation location() const noexcept { return m_location; }
};

class Error: Declaration {

};

class ExternDef : Declaration {
  Identifier m_extern_name;
  FunctionSignature m_function_signature;
};

} // namespace hades

#endif // HADES_DECLARATION_H
