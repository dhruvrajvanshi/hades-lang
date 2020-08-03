//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_PARSING_PARSER_H
#define HADES_PARSING_PARSER_H

#include "hades/ast/SourceFile.h"

namespace hades {

class Parser {
public:
    SourceFile* parse_source_file();
    Declaration* parse_declaration();
};
} // namespace hades

#endif // HADES_PARSER_H
