//
// Created by dhruv on 06/08/20.
//

#include "ParserImpl.h"
#include "Token.h"

namespace hades {

using tt = Token::Kind;

ParserImpl::ParserImpl(core::Context *ctx, const fs::path *path)
    : m_ctx(ctx), m_path(path), m_token_buffer(Lexer(path)) {}

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
  return current_token().is(kind);
}

auto ParserImpl::current_token() const -> const Token & {
  return m_token_buffer.current_token();
}

auto ParserImpl::expect(Token::Kind kind) -> Token {
  if (!at(kind)) {
    unimplemented("Unexpected token");
  }
  return advance();
}

auto ParserImpl::advance() -> Token {
  return m_token_buffer.advance();
}

auto ParserImpl::parse_identifier() -> Identifier {
  auto token = expect(tt::ID);
  return Identifier(token.location(), ctx().intern_string(token.text()));
}

auto ParserImpl::ctx() const noexcept -> core::Context & { return *m_ctx; }

} // namespace hades
