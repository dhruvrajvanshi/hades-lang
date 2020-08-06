//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_DECLARATION_H
#define HADES_DECLARATION_H

#include "hades/ast/FunctionSignature.h"
#include "hades/ast/Identifier.h"
#include "hades/core/location.h"

namespace hades {
class Declaration {
public:
  enum class Kind;

protected:
  SourceLocation m_location;
  Kind m_kind;

  Declaration(SourceLocation location, Kind kind) noexcept;

public:
  auto location() const noexcept -> const SourceLocation &;

  auto kind() const noexcept -> Kind;

  enum class Kind {
    ERROR,
    EXTERN_DEF,
    STRUCT_DEF,
  };
};

class Error : Declaration {};

class ExternDef : public Declaration {
  Identifier m_extern_name;
  FunctionSignature m_function_signature;
};

class StructMember;
class StructDef : public Declaration {
  Identifier m_name;
  SmallVec<const StructMember *, 8> m_members;

public:
  StructDef(SourceLocation location, Identifier name,
            SmallVec<const StructMember *, 8> members) noexcept;

  auto identifier() const noexcept -> const Identifier &;

  auto members() const noexcept -> ArrayRef<const StructMember *>;
};
class StructMember {};
class StructField : public StructMember {
  Identifier m_name;
  Optional<const Type *> m_type;

public:
};

} // namespace hades

#endif // HADES_DECLARATION_H
