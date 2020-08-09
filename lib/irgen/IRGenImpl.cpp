//
// Created by dhruv on 08/08/20.
//

#include "IRGenImpl.h"
#include "hades/ast/Declaration.h"
#include "hades/ast/Type.h"
#include "llvm/Support/raw_ostream.h"

namespace hades {
using t = IRGenImpl;

auto t::lower_source_file(const SourceFile &source_file) -> void {
  for (const auto *declaration : source_file.declarations()) {
    lower_declaration(*declaration);
  }
}

auto t::lower_declaration(const Declaration &decl) -> void {
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

auto t::lower_extern_def(const ExternDef &def) -> void {}

auto t::get_extern_def_callee(const ExternDef &def) -> llvm::FunctionCallee * {
  unimplemented();
}

auto t::get_function_signature_type(const FunctionSignature &signature)
    -> llvm::FunctionType * {
  assert(signature.return_type().hasValue());
  auto *return_type = lower_type(*signature.return_type().getValue());
  auto param_types =
      allocator().allocate_array_ref<const Param *, llvm::Type *>(
          signature.params(), [this](const Param *param) -> llvm::Type * {
            return lower_type(*param->type().getValue());
          });
  return llvm::FunctionType::get(return_type, param_types,
                                 /* isVarArgs */ false);
}

auto t::lower_function_def(const FunctionDef &def) -> void {
  auto *type = get_function_signature_type(def.signature());
  auto name = def.signature().name().name().as_string_ref();
  auto *function = llvm::Function::Create(
      type, llvm::GlobalValue::LinkageTypes::CommonLinkage, 0, name,
      &llvm_module());
  auto *basic_block = llvm::BasicBlock::Create(m_llvm_ctx, "entry", function);
  builder().SetInsertPoint(basic_block);

  for (auto *statement : def.body().statements()) {
    lower_statement(*statement);
  }
}

auto t::lower_statement(const Statement &statement) -> void {
  switch (statement.kind()) {
  case Statement::Kind::ERROR:
    llvm_unreachable("");
  case Statement::Kind::EXPRESSION:
    lower_expression_statement(*statement.as<ExpressionStatement>());
    return;
  case Statement::Kind::VAL:
    lower_val_statement(*statement.as<ValStatement>());
    return;
  }
  llvm_unreachable("");
}
auto t::lower_expression_statement(const ExpressionStatement &) -> void {
  unimplemented();
}

auto t::lower_val_statement(const ValStatement &statement) -> void {
  auto *initializer = lower_expression(statement.initializer());
  auto *ptr = builder().CreateAlloca(initializer->getType(), nullptr,
                                     statement.name().as_string_ref());
  builder().CreateStore(initializer, ptr, /* isVolatile */ false);
}

auto t::lower_expression(const Expression &expr) -> llvm::Value * {
  unimplemented();
}

auto t::lower_struct_def(const StructDef &) -> void {}

auto t::lower_type(const Type &type) -> llvm::Type * {
  switch (type.kind()) {
  case Type::Kind::VAR: {
    auto type_var = type.as<type::Var>();
    auto resolved = m_ctx->type_resolver().resolve_type_var(type_var);
    if (resolved.is<StructDef>()) {
      auto &struct_def = resolved.as<StructDef>();
      return get_struct_def_type(struct_def);
    }
    if (resolved.is<TypeResolutionResult::Int>()) {
      auto &[width, is_signed] = resolved.as<TypeResolutionResult::Int>();
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
    auto &ptr_type = type.as<type::Pointer>();
    return llvm::PointerType::get(lower_type(*ptr_type.pointee()), 0);
  };
  case Type::Kind::INT: {
    auto int_ty = type.as<type::Int>();
    return llvm::IntegerType::get(m_llvm_ctx, int_ty.width());
  }
  }
  llvm_unreachable("");
}

auto t::get_struct_def_type(const StructDef &struct_def) -> llvm::Type * {
  if (!m_struct_def_types.contains(&struct_def)) {
    auto *struct_type = llvm::StructType::create(m_llvm_ctx);
    struct_type->setName(struct_def.identifier().name().as_string_ref());
    auto field_types = Vec<llvm::Type *>();
    for (auto *member : struct_def.members()) {
      switch (member->kind()) {
      case StructMember::Kind::FIELD: {
        auto *field = static_cast<const StructField *>(member);
        assert(field->type().hasValue());
        auto *declared_type = field->type().getValue();
        field_types.push_back(lower_type(*declared_type));
      }
      }
    }
    struct_type->setBody(allocator().copy_items(field_types));
    m_struct_def_types.insert({&struct_def, struct_type});
  }

  return m_struct_def_types.at(&struct_def);
}

IRGenImpl::IRGenImpl(core::Context *ctx) noexcept : m_ctx{ctx} {}

auto IRGenImpl::builder() -> llvm::IRBuilder<> & { return m_builder; }

auto IRGenImpl::llvm_module() -> llvm::Module & { return m_llvm_module; }

} // namespace hades
