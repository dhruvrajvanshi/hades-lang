//
// Created by dhruv on 06/08/20.
//

#ifndef HADES_PARSERIMPL_H
#define HADES_PARSERIMPL_H
#include "hades/base.h"
#include "Lexer.h"
#include "Token.h"
#include "hades/ast/SourceFile.h"
#include "hades/core/Interner.h"

namespace hades {

template <int max_lookahead>
class TokenBuffer {
  Lexer m_lexer;
  Token m_buffer[max_lookahead];
  int m_current = 0;

public:
  TokenBuffer(Lexer lexer) noexcept
      : m_lexer{std::move(lexer)}, m_buffer{m_lexer.next_token(),
                                            m_lexer.next_token(),
                                            m_lexer.next_token(),
                                            m_lexer.next_token()} {}
  auto current_token() const -> const Token& {
    return m_buffer[m_current];
  }

  auto advance() -> Token {
    auto result = current_token();
    m_buffer[m_current] = m_lexer.next_token();
    m_current = (m_current + 1) % max_lookahead;
    return result;
  }

  template <int offset>
  auto peek() -> const Token& {
    static_assert(offset < max_lookahead, "Tried to peek past max lookahead $maxLookahead");
    return m_buffer[(m_current + offset) % max_lookahead];
  }
};

#define MAX_LOOKAHEAD 4
class ParserImpl {
  using tt = Token::Kind;
  const fs::path *m_path;
  BumpPtrAllocator* m_allocator;
  Interner* m_interner;
  TokenBuffer<MAX_LOOKAHEAD> m_token_buffer;
public:
  ParserImpl(BumpPtrAllocator*, Interner*, const fs::path *m_path);
  ~ParserImpl() = default;
  HADES_DELETE_COPY(ParserImpl)
  HADES_DELETE_MOVE(ParserImpl)

  auto parse_source_file() -> const SourceFile *;

  auto parse_declaration() -> const Declaration *;

private:
  auto allocator() -> BumpPtrAllocator &;

  auto at(Token::Kind kind) const -> bool;

  auto current_token() const -> const Token&;

  template <typename T, typename ...Args>
  auto allocate(Args&&... args) -> T* {
    auto* mem = allocator().allocate(sizeof(T), alignof(T));
    return new(mem) T(std::forward<Args>(args)...);
  }

  auto expect(Token::Kind kind) -> Token;

  auto parse_block() -> const Block*;

  auto parse_statement() -> const Statement*;
  auto parse_val_statement() -> const ValStatement*;
  auto parse_expression_statement() -> const ExpressionStatement*;
  auto parse_return_statement() -> const ReturnStatement *;

  auto parse_expression() -> const Expression*;
  auto parse_int_literal() -> const IntLiteral*;
  auto parse_var_expression() -> const Expression*;
  auto parse_expression_tail(const Expression* head) -> const Expression*;
  auto parse_arg() -> const Arg*;

  auto parse_function_signature() -> const FunctionSignature*;
  auto parse_function_signature_param(u8 index) -> const Param*;

  auto parse_extern_def() -> const ExternDef*;

  auto parse_function_def() -> const FunctionDef*;

  auto parse_struct_def() -> const StructDef*;
  auto parse_struct_member() -> const StructMember*;
  auto parse_struct_field() -> const StructField*;

  auto parse_optional_type_annotation() -> Optional<const TypeAnnotation *>;
  auto parse_type() -> const TypeAnnotation *;
  auto parse_var_type() -> const type_annotation::Var*;
  auto parse_pointer_type() -> const type_annotation::Pointer*;

  template <int offset>
  auto peek() -> const Token& {
    static_assert(offset < MAX_LOOKAHEAD, "Tried to look past MAX_LOOKAHEAD");
    return m_token_buffer.peek<offset>();
  }

  template<typename T1, typename T2>
  auto make_location(
      T1 value_1,
      T2 value_2) -> SourceLocation {
    auto start_pos = get_location(value_1).start();
    auto stop_pos = get_location(value_2).stop();
    return {get_location(value_1).path(), start_pos, stop_pos};
  }

  auto advance() -> Token;

  auto parse_identifier() -> Identifier;
};
#undef MAX_LOOKAHEAD

} // namespace hades

#endif // HADES_PARSERIMPL_H
