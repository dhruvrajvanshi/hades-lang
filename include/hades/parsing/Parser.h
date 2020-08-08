//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_PARSING_PARSER_H
#define HADES_PARSING_PARSER_H

#include "hades/ast/SourceFile.h"
#include "hades/base.h"
#include "hades/context/Context.h"

namespace hades {

class ParserImpl;
class Parser {
  UniquePtr<ParserImpl> m_impl;

public:
  Parser(core::Context* ctx, const fs::path *path);
  ~Parser() noexcept;

  auto parse_source_file() -> const SourceFile *;

private:
  auto self() -> ParserImpl &;
};
} // namespace hades

#endif // HADES_PARSER_H
