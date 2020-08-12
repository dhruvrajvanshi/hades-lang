set(LIBHADES_PARSING_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")
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
target_link_libraries(libhades_parsing
        libhades_base
        libhades_core
        libhades_ast
        )
target_include_directories(libhades_parsing
        PUBLIC
        ${LIBHADES_PARSING_INCLUDE_DIRS})
