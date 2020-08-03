//
// Created by dhruv on 03/08/20.
//

#include "ContextImpl.h"

namespace hades::core {
auto ContextImpl::run() -> int {
  return evaluate(req::BuildObjectFileRequest(args()));
}

auto ContextImpl::evaluate(req::BuildObjectFileRequest request) -> int {
  auto flags_result = evaluate(req::GetBuildFlagsRequest(request.args));
  if (flags_result.is_error()) {
    auto error = flags_result.get_error();
    std::cerr << error << std::endl;
    return 1;
  }
  auto flags = flags_result.get_data();
  unimplemented();
}
auto ContextImpl::evaluate(req::GetBuildFlagsRequest flags_request) -> Result<CommandLineFlags, FlagParseError> {
  return CommandLineFlags::parse(*flags_request.args);
}
auto ContextImpl::args() const -> const Vec<String> * { return &m_args; }

}; // namespace hades::core
