//
// Created by dhruv on 06/08/20.
//
#include "hades/parsing/Parser.h"
#include "ParserImpl.h"

namespace hades {

Parser::Parser(BumpPtrAllocator* allocator, Interner* interner, const fs::path *path)
    : m_impl{new ParserImpl(allocator, interner, path)} {}

auto Parser::parse_source_file() -> const SourceFile * {
  return self().parse_source_file();
}

auto Parser::self() -> ParserImpl & { return *m_impl; }
Parser::~Parser() noexcept = default;

} // namespace hades
