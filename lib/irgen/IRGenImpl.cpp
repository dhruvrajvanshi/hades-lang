//
// Created by dhruv on 08/08/20.
//

#include "IRGenImpl.h"

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

auto t::lower_extern_def(const ExternDef &) -> void {
  unimplemented();
}

auto t::lower_function_def(const FunctionDef &) -> void {
  unimplemented();
}

auto t::lower_struct_def(const StructDef &) -> void {
  unimplemented();
}

IRGenImpl::IRGenImpl(core::Context *ctx) noexcept : m_ctx{ctx} {}

auto IRGenImpl::builder() -> llvm::IRBuilder<> & { return m_builder; }

auto IRGenImpl::llvm_module() -> llvm::Module & { return m_llvm_module; }

} // namespace hades
