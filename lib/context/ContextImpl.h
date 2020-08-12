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
#include "hades/core/Interner.h"

namespace hades::core {
namespace req = requests;
class ContextImpl {

  RequestEvaluator m_evaluator;
  CommandLineFlags m_flags;
  BumpPtrAllocator m_allocator;
  Context *m_ctx;
  Map<String, const SourceFile *> m_source_files{};
  UniquePtr<NameResolver> m_name_resolver;
  UniquePtr<Typer> m_typer;
  Interner m_interner;

public:
  ContextImpl() = delete;
  ~ContextImpl() = default;
  ContextImpl(Context *ctx, CommandLineFlags flags)
      : m_evaluator{}, m_flags{flags}, m_ctx{ctx},
        m_name_resolver{nullptr}, m_typer{nullptr}, m_interner{Interner(&m_allocator)} {}
  HADES_DELETE_MOVE(ContextImpl)
  HADES_DELETE_COPY(ContextImpl)

  auto run() -> int;
  auto evaluate(req::BuildObjectFileRequest) -> int;
  auto flags() const -> const CommandLineFlags &;

  auto allocator() -> BumpPtrAllocator &;

  auto name_resolver() -> NameResolver &;

  auto typer() -> Typer &;

  auto interner() -> Interner &;

  auto get_source_file(const fs::path &) -> const SourceFile &;
};
static_assert(std::is_move_constructible_v<ContextImpl>);
static_assert(std::is_move_assignable_v<ContextImpl>);

} // namespace hades::core

#endif // HADES_CONTEXTIMPL_H
