//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_DECLARATION_H
#define HADES_DECLARATION_H

#include "Block.h"
#include "FunctionSignature.h"
#include "Identifier.h"
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
  HADES_DELETE_COPY(Declaration)
  HADES_DELETE_MOVE(Declaration)
  auto location() const noexcept -> const SourceLocation &;

  auto kind() const noexcept -> Kind;

  enum class Kind {
    ERROR,
    EXTERN_DEF,
    STRUCT_DEF,
    FUNCTION_DEF,
  };

  template <typename T>
  auto as() const -> const T& {
    assert(kind() == T::kind);
    return static_cast<const T&>(*this);
  }
};

class Error : Declaration {};

class ExternDef : public Declaration {
  const FunctionSignature *m_signature;
  Identifier m_extern_name;

public:
  HADES_DELETE_COPY(ExternDef)
  HADES_DELETE_MOVE(ExternDef)
  static constexpr Kind kind = Kind::EXTERN_DEF;
  ExternDef(SourceLocation location, const FunctionSignature *signature,
            Identifier extern_name) noexcept;

  auto signature() const -> const FunctionSignature &;

  auto extern_name() const -> const Identifier &;
};

class FunctionDef : public Declaration {
  const FunctionSignature *m_signature;
  const Block* m_body;

public:
  static constexpr Kind kind = Kind::FUNCTION_DEF;
  FunctionDef(const FunctionSignature* signature, const Block* body) noexcept;
  auto signature() const -> const FunctionSignature& { return *m_signature; }
  auto body() const -> const Block&;
};

class StructMember;
class StructDef : public Declaration {
public:
  static constexpr Kind kind = Kind::STRUCT_DEF;
  using Members = SmallVec<const StructMember *, 8>;

private:
  Identifier m_name;
  Members m_members;

public:
  StructDef(SourceLocation location, Identifier name,
            SmallVec<const StructMember *, 8> members) noexcept;

  auto identifier() const noexcept -> const Identifier &;

  auto members() const noexcept -> ArrayRef<const StructMember *>;
};
class StructMember {
public:
  enum class Kind;
private:
  Kind m_kind;
  SourceLocation m_location;

public:
  StructMember(SourceLocation location, Kind kind) noexcept;
  auto location() const -> SourceLocation;
  auto kind() const -> Kind;
  enum class Kind {
    FIELD
  };
};
class StructField : public StructMember {
  Identifier m_name;
  Optional<const Type *> m_type;

public:
  StructField(SourceLocation location, Identifier name,
              Optional<const Type *> type) noexcept;
  auto type() const -> Optional<const Type*>;
};

} // namespace hades

#endif // HADES_DECLARATION_H
