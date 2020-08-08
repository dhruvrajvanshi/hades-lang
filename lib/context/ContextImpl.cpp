//
// Created by dhruv on 03/08/20.
//

#include "ContextImpl.h"
#include "hades/irgen/IRGen.h"
#include "hades/parsing/Parser.h"

namespace hades::core {
auto ContextImpl::run() -> int {
  return evaluate(req::BuildObjectFileRequest(&flags()));
}

auto ContextImpl::evaluate(req::BuildObjectFileRequest request) -> int {
  auto irgen = IRGen(m_ctx);
  for (auto &source_file_path : flags().sources()) {
    const auto& source_file = get_source_file(source_file_path);
    irgen.lower_source_file(&source_file);
  }
  unimplemented();
}

auto ContextImpl::flags() const -> const CommandLineFlags & { return m_flags; }

auto ContextImpl::allocator() -> llvm::BumpPtrAllocator & {
  return m_allocator;
}

auto ContextImpl::intern_string(StringView text) -> InternedString {
  if (m_interned_strings.find(text) == m_interned_strings.cend()) {
    auto *buffer =
        (char *)allocator().Allocate(text.length() + 1, alignof(char));
    buffer[text.length()] = '\0';
    memcpy((void *)buffer, (void *)text.begin(), text.length());
    auto view = StringView(buffer, text.length());
    m_interned_strings.insert({view, {buffer, text.length()}});
  }
  const auto &item = m_interned_strings.find(text);
  auto interned_str = item->second;
  return interned_str;
}

auto ContextImpl::type_resolver() -> TypeResolver & { return *m_type_resolver; }

auto ContextImpl::get_source_file(const fs::path & path) -> const SourceFile& {
  if (!m_source_files.contains(fs::absolute(path))) {
    const auto *path_ptr = &path;
    auto parser = Parser(m_ctx, path_ptr);
    m_source_files.insert({ path, parser.parse_source_file() });
  }
  return *m_source_files[fs::absolute(path)];
}

}; // namespace hades::core
