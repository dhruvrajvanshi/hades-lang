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

auto ContextImpl::allocator() -> BumpPtrAllocator & {
  return m_allocator;
}


auto ContextImpl::name_resolver() -> NameResolver & {
  if (m_name_resolver == nullptr) {
    m_name_resolver.reset(new NameResolver(m_ctx));
  }
  return *m_name_resolver;
}

auto ContextImpl::get_source_file(const fs::path & path) -> const SourceFile& {
  if (!m_source_files.contains(fs::absolute(path))) {
    const auto *path_ptr = &path;
    auto parser = Parser(&allocator(), &interner(), path_ptr);
    m_source_files.insert({ fs::absolute(path), parser.parse_source_file() });
  }
  return *m_source_files[fs::absolute(path)];
}

auto ContextImpl::typer() -> Typer & {
  if (m_typer == nullptr) {
    m_typer.reset(new Typer(&name_resolver(), &allocator()));
  }
  return *m_typer;
}

auto ContextImpl::interner() -> Interner & { return m_interner; }

}; // namespace hades::core
