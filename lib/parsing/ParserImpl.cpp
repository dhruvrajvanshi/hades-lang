//
// Created by dhruv on 06/08/20.
//

#include "ParserImpl.h"
#include "Token.h"

namespace hades {

using tt = Token::Kind;

ParserImpl::ParserImpl(core::Context *ctx, const fs::path *path)
    : m_ctx(ctx), m_path(path), m_lexer(path),
      m_current_token(m_lexer.next_token()) {}

auto ParserImpl::parse_source_file() -> const SourceFile * {
  auto declarations = Vec<const Declaration *>();
  while (!at(tt::ENDF)) {
    declarations.push_back(parse_declaration());
  }
  return allocate<SourceFile>(std::move(declarations));
}

auto ParserImpl::allocator() -> llvm::BumpPtrAllocator & {
  return m_ctx->allocator();
}
auto ParserImpl::at(Token::Kind kind) const -> bool {
  return m_current_token.is(kind);
}

} // namespace hades
