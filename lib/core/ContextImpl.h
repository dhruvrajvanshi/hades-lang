//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_CONTEXTIMPL_H
#define HADES_CONTEXTIMPL_H

#include "hades/core/RequestEvaluator.h"
#include "hades/requests.h"
#include "hades/base.h"
#include "hades/core/Context.h"

namespace hades::core {
namespace req = requests;
class ContextImpl {

  RequestEvaluator m_evaluator;
  CommandLineFlags m_flags;
  llvm::BumpPtrAllocator m_allocator;
  Context* m_ctx = nullptr;
  Map<StringView, InternedString> m_interned_strings;

public:
  static auto from_args(const Vec<String>&) noexcept -> Result<ContextImpl, FlagParseError>;
  ContextImpl() = delete;
  ~ContextImpl() = default;
  ContextImpl(CommandLineFlags flags) : m_evaluator{}, m_flags{flags} {}
  HADES_DEFAULT_MOVE(ContextImpl)
  HADES_DELETE_COPY(ContextImpl)

  auto set_ctx_ptr(Context*) noexcept -> void;

  auto run() -> int;
  auto evaluate(req::BuildObjectFileRequest) -> int;
  auto flags() const -> const CommandLineFlags&;

  auto allocator() -> llvm::BumpPtrAllocator &;

  auto intern_string(StringView text) -> InternedString;
};
static_assert(std::is_move_constructible_v<ContextImpl>);
static_assert(std::is_move_assignable_v<ContextImpl>);

} // namespace hades::core

#endif // HADES_CONTEXTIMPL_H
