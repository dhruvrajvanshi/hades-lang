
set(LIBHADES_SOURCES
        core/Context.cpp
        core/ContextImpl.cpp
        core/location.cpp
        core/CommandLineFlags.cpp

        base/Result.cpp
        )

add_library(libhades ${LIBHADES_SOURCES} ${HADES_INCLUDE_DIRS})
target_link_libraries(libhades LLVM-10 libhades_parsing libhades_ast)
target_include_directories(libhades PUBLIC ${HADES_INCLUDE_DIRS})

set(LIBHADES_PARSING_SOURCES
        parsing/ParserImpl.cpp
        parsing/Parser.cpp
        parsing/Token.cpp
        parsing/Lexer.cpp)
add_library(libhades_parsing ${LIBHADES_PARSING_SOURCES})
target_include_directories(libhades_parsing PUBLIC ${HADES_INCLUDE_DIRS})

set(LIBHADES_AST_SOURCES
        ast/SourceFile.cpp
        ast/Identifier.cpp)
add_library(libhades_ast ${LIBHADES_AST_SOURCES})
target_include_directories(libhades_ast PUBLIC ${HADES_INCLUDE_DIRS})

