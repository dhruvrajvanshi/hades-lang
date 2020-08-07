set(LIBHADES_CORE_SOURCES
        core/ContextImpl.cpp
        core/Context.cpp
        core/location.cpp
        core/CommandLineFlags.cpp
        core/InternedString.cpp

        base/Result.cpp
        base/sys.cpp
        )

add_library(libhades_core ${LIBHADES_CORE_SOURCES} ${HADES_INCLUDE_DIRS})
target_link_libraries(libhades_core LLVM-10)
target_include_directories(libhades_core PUBLIC ${HADES_INCLUDE_DIRS})

set(LIBHADES_PARSING_SOURCES
        parsing/ParserImpl.cpp
        parsing/Parser.cpp
        parsing/Token.cpp
        parsing/Lexer.cpp
        parsing/parse_declaration.cpp
        parsing/parse_type.cpp
        )
add_library(libhades_parsing ${LIBHADES_PARSING_SOURCES})
target_include_directories(libhades_parsing PUBLIC ${HADES_INCLUDE_DIRS})
target_link_libraries(libhades_parsing libhades_core libhades_ast)

set(LIBHADES_AST_SOURCES
        ast/SourceFile.cpp
        ast/Identifier.cpp
        ast/Declaration.cpp
        ast/Type.cpp
        )
add_library(libhades_ast ${LIBHADES_AST_SOURCES})
target_include_directories(libhades_ast PUBLIC ${HADES_INCLUDE_DIRS})

