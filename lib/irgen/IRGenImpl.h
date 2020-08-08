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

namespace hades {

class IRGenImpl {
  core::Context* m_ctx;
  llvm::LLVMContext m_llvm_ctx {};
  llvm::Module m_llvm_module {{"hades"}, m_llvm_ctx};
  llvm::IRBuilder<> m_builder { m_llvm_ctx };
  llvm::BumpPtrAllocator m_allocator;

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

  auto lower_type(const Type&) -> llvm::Type*;

  template <typename T, typename ...Args>
  auto allocate(Args&&... args) -> T* {
    auto* mem = m_allocator.Allocate(sizeof(T), alignof(T));
    return new(mem) T(std::forward<Args>(args)...);
  }

  template <typename T1, typename T2>
  auto allocate_array_ref(ArrayRef<T1> input, std::function<T2(T1)> f) -> ArrayRef<T2> {
      T2* begin = m_allocator.Allocate<T2>();
      T2* current = begin;
      for (auto& item : input) {
          *current = f(item);
          current++;
      }
      return ArrayRef<T2>(begin, input.size());
  }
};

} // namespace hades

#endif // HADES_IRGENIMPL_H
