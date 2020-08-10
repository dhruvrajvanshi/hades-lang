//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_CONTEXTIMPL_H
#define HADES_CONTEXTIMPL_H

#include "hades/analysis/Typer.h"
#include "hades/base.h"
#include "hades/context/Context.h"
#include "hades/core/RequestEvaluator.h"
#include "hades/requests.h"

namespace hades::core {
namespace req = requests;
class ContextImpl {

  RequestEvaluator m_evaluator;
  CommandLineFlags m_flags;
  llvm::BumpPtrAllocator m_allocator;
  Context *m_ctx;
  Map<StringView, InternedString> m_interned_strings{};
  Map<String, const SourceFile *> m_source_files{};
  UniquePtr<NameResolver> m_name_resolver;
  UniquePtr<Typer> m_typer;

public:
  ContextImpl() = delete;
  ~ContextImpl() = default;
  ContextImpl(Context *ctx, CommandLineFlags flags)
      : m_evaluator{}, m_flags{flags}, m_ctx{ctx},
        m_name_resolver{nullptr}, m_typer{nullptr} {}
  HADES_DELETE_MOVE(ContextImpl)
  HADES_DELETE_COPY(ContextImpl)

  auto run() -> int;
  auto evaluate(req::BuildObjectFileRequest) -> int;
  auto flags() const -> const CommandLineFlags &;

  auto allocator() -> llvm::BumpPtrAllocator &;

  auto intern_string(StringView text) -> InternedString;

  auto type_resolver() -> NameResolver &;

  auto typer() -> Typer &;

  auto get_source_file(const fs::path &) -> const SourceFile &;
};
static_assert(std::is_move_constructible_v<ContextImpl>);
static_assert(std::is_move_assignable_v<ContextImpl>);

} // namespace hades::core

#endif // HADES_CONTEXTIMPL_H
