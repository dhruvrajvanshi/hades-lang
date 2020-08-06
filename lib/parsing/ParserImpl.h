//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_PARSERIMPL_H
#define HADES_PARSERIMPL_H
#include "hades/ast/SourceFile.h"
#include "hades/base.h"
#include "hades/core/Context.h"
#include "Token.h"
#include "Lexer.h"

namespace hades {

class ParserImpl {
  using tt = Token::Kind;
  const fs::path *m_path;
  core::Context* m_ctx;
  Lexer m_lexer;

  Token m_current_token;

public:
  ParserImpl(core::Context *m_ctx, const fs::path *m_path);
  ~ParserImpl() = default;
  auto parse_source_file() -> const SourceFile *;

  auto parse_declaration() -> const Declaration *;

private:
  auto allocator() -> llvm::BumpPtrAllocator &;

  auto at(Token::Kind kind) const -> bool;

  auto current_token() const -> const Token&;

  template <typename T, typename ...Args>
  auto allocate(Args&&... args) -> T* {
    auto* mem = allocator().Allocate(sizeof(T), alignof(T));
    return new(mem) T(std::forward<Args>(args)...);
  }

  auto expect(Token::Kind kind) -> Token;

  auto parse_struct_def() -> const StructDef*;
  auto advance() -> Token;

  auto parse_identifier() -> Identifier;

  auto ctx() const noexcept -> core::Context&;
};

} // namespace hades

#endif // HADES_PARSERIMPL_H
