//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_CONTEXTIMPL_H
#define HADES_CONTEXTIMPL_H

#include "hades/core/RequestEvaluator.h"
#include "hades/requests.h"
#include "hades/base.h"

namespace hades::core {
namespace req = requests;
class ContextImpl {

  RequestEvaluator m_evaluator;
  CommandLineFlags m_flags;

public:
  static auto from_args(const Vec<String>&) noexcept -> Result<ContextImpl, FlagParseError>;
  ContextImpl() = delete;
  ~ContextImpl() = default;
  ContextImpl(CommandLineFlags flags) : m_evaluator{}, m_flags{flags} {}
  HADES_DEFAULT_MOVE(ContextImpl)
  HADES_DELETE_COPY(ContextImpl)

  auto run() -> int;
  auto evaluate(req::BuildObjectFileRequest) -> int;
  auto flags() const -> const CommandLineFlags&;
};
static_assert(std::is_move_constructible_v<ContextImpl>);
static_assert(std::is_move_assignable_v<ContextImpl>);

} // namespace hades::core

#endif // HADES_CONTEXTIMPL_H
