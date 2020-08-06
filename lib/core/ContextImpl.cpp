//
// Created by dhruv on 03/08/20.
//

#include "ContextImpl.h"
#include "hades/parsing/Parser.h"

namespace hades::core {
auto ContextImpl::run() -> int {
  return evaluate(req::BuildObjectFileRequest(&flags()));
}

auto ContextImpl::evaluate(req::BuildObjectFileRequest request) -> int {
  for (auto& source_file_path : flags().sources()) {
    const auto* path_ptr = &source_file_path;
    auto parser = Parser(m_ctx, path_ptr);
    const auto* source_file = parser.parse_source_file();
  }
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

auto ContextImpl::allocator() -> llvm::BumpPtrAllocator & {
  return m_allocator;
}
auto ContextImpl::set_ctx_ptr(Context * ctx) noexcept -> void {
  m_ctx = ctx;
}

}; // namespace hades::core
