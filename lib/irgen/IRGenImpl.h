//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_IRGENIMPL_H
#define HADES_IRGENIMPL_H

#include "hades/base.h"
#include "hades/context/Context.h"
#include "hades/ast/SourceFile.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DerivedTypes.h"

namespace hades {

class IRGenImpl {
  core::Context* m_ctx;
  llvm::LLVMContext m_llvm_ctx {};
  llvm::Module m_llvm_module {{"hades"}, m_llvm_ctx};
  llvm::IRBuilder<> m_builder { m_llvm_ctx };
  BumpPtrAllocator m_allocator;
  Map<const ExternDef*, llvm::FunctionCallee*> m_extern_def_callees{};
  Map<const StructDef*, llvm::Type*> m_struct_def_types{};
  u64 m_next_name = 0;

public:
  IRGenImpl(core::Context* ctx) noexcept;
  ~IRGenImpl() noexcept = default;

  auto lower_source_file(const SourceFile&) -> void;
private:
  auto builder() -> llvm::IRBuilder<>&;
  auto llvm_module() -> llvm::Module&;

  auto lower_declaration(const Declaration&) -> void;
  auto lower_struct_def(const StructDef&) -> void;
  auto lower_extern_def(const ExternDef&) -> void;
  auto lower_function_def(const FunctionDef&) -> void;

  auto lower_type(const TypeAnnotation &) -> llvm::Type*;

  auto get_struct_def_type(const StructDef&) -> llvm::Type*;

  auto get_extern_def_var(const ExternDef*) -> llvm::Function*;

  auto get_function_signature_type(const FunctionSignature*) -> llvm::FunctionType*;

  auto lower_statement(const Statement&) -> void;
  auto lower_expression(const Expression&) -> llvm::Value*;
  auto lower_var_expression(const VarExpression&) -> llvm::Value*;
  auto lower_int_literal(const IntLiteral&) -> llvm::Value*;
  auto lower_call(const Call&) -> llvm::Value*;

  auto lower_expression_statement(const ExpressionStatement&) -> void;
  auto lower_val_statement(const ValStatement&) -> void;

  auto allocator() -> BumpPtrAllocator& { return m_allocator; }

  auto make_unique_name() -> InternedString;
};

} // namespace hades

#endif // HADES_IRGENIMPL_H
