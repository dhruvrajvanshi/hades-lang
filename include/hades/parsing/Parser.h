//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_PARSING_PARSER_H
#define HADES_PARSING_PARSER_H

#include "hades/ast/SourceFile.h"

namespace hades {

class ParserImpl;
class Parser {
public:
  SourceFile *parse_source_file();
  Declaration *parse_declaration();

private:
};
} // namespace hades

#endif // HADES_PARSER_H
