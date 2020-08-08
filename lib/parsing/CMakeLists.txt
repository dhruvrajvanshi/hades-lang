
set(LIBHADES_PARSING_SOURCES
        ParserImpl.cpp
        Parser.cpp
        Token.cpp
        Lexer.cpp
        parse_declaration.cpp
        parse_type.cpp
        parse_statement.cpp
        parse_expression.cpp
        )
add_library(libhades_parsing ${LIBHADES_PARSING_SOURCES})
target_include_directories(libhades_parsing PUBLIC ${HADES_INCLUDE_DIRS})
target_link_libraries(libhades_parsing libhades_core libhades_ast)
