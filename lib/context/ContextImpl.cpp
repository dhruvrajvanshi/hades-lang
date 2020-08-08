//
// Created by dhruv on 03/08/20.
//

#include "ContextImpl.h"
#include "hades/parsing/Parser.h"
#include "hades/irgen/IRGen.h"

namespace hades::core {
auto ContextImpl::run() -> int {
  return evaluate(req::BuildObjectFileRequest(&flags()));
}

auto ContextImpl::evaluate(req::BuildObjectFileRequest request) -> int {
  auto irgen = IRGen(m_ctx);
  for (auto &source_file_path : flags().sources()) {
    const auto *path_ptr = &source_file_path;
    auto parser = Parser(m_ctx, path_ptr);
    const auto *source_file = parser.parse_source_file();

    irgen.lower_source_file(source_file);

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

auto ContextImpl::set_ctx_ptr(Context *ctx) noexcept -> void { m_ctx = ctx; }

auto ContextImpl::intern_string(StringView text) -> InternedString {
  if (m_interned_strings.find(text) == m_interned_strings.cend()) {
    auto* buffer = (char*) allocator().Allocate(text.length() + 1, alignof(char));
    buffer[text.length()] = '\0';
    memcpy((void*) buffer, (void*)text.begin(), text.length());
    auto view = StringView(buffer, text.length());
    m_interned_strings.insert({view, {buffer, text.length()}});
  }
  const auto& item = m_interned_strings.find(text);
  auto interned_str = item->second;
  return interned_str;
}

}; // namespace hades::core
