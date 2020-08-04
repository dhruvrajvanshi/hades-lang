//
// Created by dhruv on 03/08/20.
//

#include "ContextImpl.h"

namespace hades::core {
auto ContextImpl::run() -> int {
  return evaluate(req::BuildObjectFileRequest(&flags()));
}

auto ContextImpl::evaluate(req::BuildObjectFileRequest request) -> int {
  unimplemented();
}

auto ContextImpl::flags() const -> const CommandLineFlags & { return m_flags; }

auto ContextImpl::from_args(const Vec<String> &args) noexcept
    -> Result<ContextImpl, FlagParseError> {
  return CommandLineFlags::parse(args).map<ContextImpl>(
      [](auto &&flags) -> ContextImpl {
        return ContextImpl(std::move(flags));
      });
}

}; // namespace hades::core
