//
// Created by dhruv on 06/08/20.
//

#include "ParserImpl.h"
namespace hades {

ParserImpl::ParserImpl(
    core::Context* ctx,
    const fs::path *path
    ) : m_ctx(ctx), m_path(path) {}
auto ParserImpl::parse_source_file() -> const SourceFile * {
  auto declarations = Vec<const Declaration*>();
  return allocate<SourceFile>(std::move(declarations));
}

auto ParserImpl::allocator() -> llvm::BumpPtrAllocator & {
  return m_ctx->allocator();
}

} // namespace hades
