//
// Created by dhruv on 08/08/20.
//

#include "IRGenImpl.h"
#include "hades/ast/Type.h"
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
  auto param_types = allocate_array_ref<const Param*, llvm::Type*>(
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
    unimplemented();
  }
  case Type::Kind::POINTER: {
    return llvm::PointerType::get(lower_type(type.as<type::Pointer>()), 0);
  };
  }
  llvm_unreachable("");
}

IRGenImpl::IRGenImpl(core::Context *ctx) noexcept : m_ctx{ctx} {}

auto IRGenImpl::builder() -> llvm::IRBuilder<> & { return m_builder; }

auto IRGenImpl::llvm_module() -> llvm::Module & { return m_llvm_module; }

} // namespace hades
