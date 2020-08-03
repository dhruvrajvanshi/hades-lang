//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_CONTEXTIMPL_H
#define HADES_CONTEXTIMPL_H

#include "hades/core/RequestEvaluator.h"
#include "hades/requests.h"

namespace hades::core {
namespace req = requests;
class ContextImpl {

  RequestEvaluator m_evaluator;
  Vec<String> m_args;

public:
  ContextImpl(Vec<String> args) : m_evaluator{}, m_args{std::move(args)} {}
  ContextImpl() = delete;
  ContextImpl(const ContextImpl&) = delete;
  auto operator=(const ContextImpl&) = delete;

  auto run() -> int;
  auto evaluate(req::BuildObjectFileRequest) -> int;
  auto evaluate(req::GetBuildFlagsRequest) -> Result<CommandLineFlags, FlagParseError>;
  auto args() const -> const Vec<String>*;
};

} // namespace hades::core

#endif // HADES_CONTEXTIMPL_H
