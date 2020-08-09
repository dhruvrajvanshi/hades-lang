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

auto t::get_extern_def_var(const ExternDef * def) -> llvm::Function * {
  if (llvm_module().getFunction(def->extern_name().as_string_ref()) == nullptr) {
    auto *type = get_function_signature_type(&def->signature());
    auto name = def->extern_name().as_string_ref();
    llvm::Function::Create(type,
                           llvm::GlobalValue::LinkageTypes::ExternalLinkage, 0,
                           name, &llvm_module());
  }

  return llvm_module().getFunction(def->extern_name().as_string_ref());
}

auto t::get_function_signature_type(const FunctionSignature * signature)
    -> llvm::FunctionType * {
  assert(signature->return_type().hasValue());
  auto& return_type_annotation = *signature->return_type().getValue();
  auto *return_type = lower_type(return_type_annotation);
  auto param_types =
      allocator().allocate_array_ref<const Param *, llvm::Type *>(
          signature->params(), [this](const Param *param) -> llvm::Type * {
            return lower_type(*param->type().getValue());
          });
  auto fn = llvm::FunctionType::get(return_type, param_types,
                                 /* isVarArgs */ false);


  return fn;
}

auto t::lower_function_def(const FunctionDef &def) -> void {
  auto *type = get_function_signature_type(&def.signature());
  auto name = def.signature().name().name().as_string_ref();
  auto *function = llvm::Function::Create(
      type, llvm::GlobalValue::LinkageTypes::CommonLinkage, 0, name,
      &llvm_module());
  auto *basic_block = llvm::BasicBlock::Create(m_llvm_ctx, "entry", function);
  builder().SetInsertPoint(basic_block);

  for (const auto *statement : def.body().statements()) {
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
auto t::lower_expression_statement(const ExpressionStatement &statement)
    -> void {
  lower_expression(statement.expression());
}

auto t::lower_val_statement(const ValStatement &statement) -> void {
  auto *initializer = lower_expression(statement.initializer());
  auto *ptr = builder().CreateAlloca(initializer->getType(), nullptr,
                                     statement.name().as_string_ref());
  builder().CreateStore(initializer, ptr, /* isVolatile */ false);
}

auto t::lower_expression(const Expression &expr) -> llvm::Value * {
  switch (expr.kind()) {
  case Expression::Kind::ERROR:
    llvm_unreachable("");
  case Expression::Kind::VAR:
    return lower_var_expression(*expr.as<VarExpression>());
  case Expression::Kind::CALL:
    return lower_call(*expr.as<Call>());
  case Expression::Kind::INT_LITERAL:
    return lower_int_literal(*expr.as<IntLiteral>());
  }
  llvm_unreachable("");
}

auto t::lower_var_expression(const VarExpression & expr) -> llvm::Value * {
  auto resolved_var = m_ctx->name_resolver().resolve_expr_var(expr);
  if (resolved_var.is<ExternDef>()) {
    const auto* extern_def = resolved_var.as<ExternDef>();
    return get_extern_def_var(extern_def);
  }
  if (resolved_var.is<FunctionDef>()) {
    unimplemented();
  }
  if (resolved_var.is<ValStatement>()) {
    unimplemented();
  }
  if (resolved_var.is<Unresolved>()) {
    m_ctx->name_resolver().resolve_expr_var(expr);
    unimplemented();
  }
  llvm_unreachable("");
}

auto t::lower_call(const Call & call) -> llvm::Value * {
  auto* callee = lower_expression(call.callee());
  auto args = Vec<llvm::Value*>();
  args.reserve(call.args().size());
  for (const auto* arg : call.args()) {
    assert(!arg->label().hasValue());
    args.push_back(lower_expression(arg->value()));
  }
  ArrayRef<llvm::Value*> args_ref = allocator().copy_items<llvm::Value*>(args);
  return builder().CreateCall(callee, args_ref, make_unique_name().as_string_ref());
}

auto t::lower_int_literal(const IntLiteral & i) -> llvm::Value * {
  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_llvm_ctx), llvm::APInt(32, i.value()));
}

auto t::lower_struct_def(const StructDef &) -> void {}

auto t::lower_type(const Type &type) -> llvm::Type * {
  switch (type.kind()) {
  case Type::Kind::VAR: {
    auto& type_var = type.as<type::Var>();
    auto resolved = m_ctx->name_resolver().resolve_type_var(type_var);
    if (resolved.is<StructDef>()) {
      const auto* struct_def = resolved.as<StructDef>();
      return get_struct_def_type(*struct_def);
    }
    if (resolved.is<NameResolutionResult::Int>()) {
      const auto [width, is_signed] = *resolved.as<NameResolutionResult::Int>();
      return llvm::IntegerType::get(m_llvm_ctx, width);
    }
    if (resolved.is<NameResolutionResult::Void>()) {
      return llvm::Type::getVoidTy(m_llvm_ctx);
    }
    if (resolved.is<Unresolved>()) {
      llvm_unreachable("");
    }
    llvm_unreachable("");
  }
  case Type::Kind::POINTER: {
    const auto &ptr_type = type.as<type::Pointer>();
    return llvm::PointerType::get(lower_type(*ptr_type.pointee()), 0);
  };
  case Type::Kind::INT: {
    auto& int_ty = type.as<type::Int>();
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
    for (const auto *member : struct_def.members()) {
      switch (member->kind()) {
      case StructMember::Kind::FIELD: {
        const auto *field = static_cast<const StructField *>(member);
        assert(field->type().hasValue());
        const auto *declared_type = field->type().getValue();
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

auto IRGenImpl::make_unique_name() -> InternedString {
  u64 value = m_next_name;
  m_next_name++;
  auto str = String("$") + std::to_string(value);
  return m_ctx->intern_string(StringView(str));
}

} // namespace hades
