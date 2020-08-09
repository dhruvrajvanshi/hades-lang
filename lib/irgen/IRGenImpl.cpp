//
// Created by dhruv on 08/08/20.
//

#include "IRGenImpl.h"
#include "hades/ast/Type.h"
#include "hades/ast/Declaration.h"
#include "llvm/Support/raw_ostream.h"

namespace hades {
using t = IRGenImpl;

auto t::lower_source_file(const SourceFile& source_file) -> void {
  for (const auto* declaration : source_file.declarations()) {
    lower_declaration(*declaration);
  }
}

auto t::lower_declaration(const Declaration & decl) -> void {
  using k = Declaration::Kind;
  switch (decl.kind()) {
  case k::EXTERN_DEF:
    return lower_extern_def(decl.as<ExternDef>());
  case k::FUNCTION_DEF:
    return lower_function_def(decl.as<FunctionDef>());
  case k::STRUCT_DEF:
    return lower_struct_def(decl.as<StructDef>());
  case k::ERROR:
    llvm_unreachable("");
  }
  llvm_unreachable("");
}

auto t::lower_extern_def(const ExternDef & def) -> void {
  assert(def.signature().return_type().hasValue());
  auto* return_type = lower_type(*def.signature().return_type().getValue());
  auto param_types = allocator().allocate_array_ref<const Param*, llvm::Type*>(
      def.signature().params(),
      [this](const Param* param) -> llvm::Type* {
        return lower_type(*param->type().getValue());
      }
  );
  const auto* fn_type = llvm::FunctionType::get(return_type, param_types, /* isVarArgs */ false);
  unimplemented();
}

auto t::lower_function_def(const FunctionDef &) -> void {
  unimplemented();
}

auto t::lower_struct_def(const StructDef &) -> void {}

auto t::lower_type(const Type & type) -> llvm::Type * {
  switch (type.kind()) {
  case Type::Kind::VAR: {
    auto type_var = type.as<type::Var>();
    auto resolved = m_ctx->type_resolver().resolve_type_var(type_var);
    if (resolved.is<StructDef>()) {
      auto& struct_def = resolved.as<StructDef>();
      return get_struct_def_type(struct_def);
    }
    if (resolved.is<TypeResolutionResult::Int>()) {
      auto& [width, is_signed] = resolved.as<TypeResolutionResult::Int>();
      return llvm::IntegerType::get(m_llvm_ctx, width);
    }
    if (resolved.is<TypeResolutionResult::Void>()) {
      return llvm::Type::getVoidTy(m_llvm_ctx);
    }
    if (resolved.is<UnresolvedType>()) {
      llvm_unreachable("");
    }
    llvm_unreachable("");
  }
  case Type::Kind::POINTER: {
    auto& ptr_type = type.as<type::Pointer>();
    return llvm::PointerType::get(lower_type(*ptr_type.pointee()), 0);
  };
  case Type::Kind::INT: {
    auto int_ty = type.as<type::Int>();
    return llvm::IntegerType::get(m_llvm_ctx, int_ty.width());
  }
  }
  llvm_unreachable("");
}

auto t::get_struct_def_type(const StructDef & struct_def) -> llvm::Type * {
  auto* struct_type = llvm::StructType::create(m_llvm_ctx);
  struct_type->setName(struct_def.identifier().name().as_string_ref());
  auto field_types = Vec<llvm::Type*>();
  for (auto* member : struct_def.members()) {
    switch (member->kind()) {
    case StructMember::Kind::FIELD: {
      auto* field = static_cast<const StructField*>(member);
      assert(field->type().hasValue());
      auto* declared_type = field->type().getValue();
      field_types.push_back(lower_type(*declared_type));
    }
    }
  }

  struct_type->setBody(allocator().copy_items(field_types));

  struct_type->print(llvm::outs());

  return struct_type;
}

IRGenImpl::IRGenImpl(core::Context *ctx) noexcept : m_ctx{ctx} {}

auto IRGenImpl::builder() -> llvm::IRBuilder<> & { return m_builder; }

auto IRGenImpl::llvm_module() -> llvm::Module & { return m_llvm_module; }

} // namespace hades
