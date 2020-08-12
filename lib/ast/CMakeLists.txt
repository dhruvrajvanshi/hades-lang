set(LIBHADES_AST_SOURCES
        SourceFile.cpp
        Identifier.cpp
        Declaration.cpp
        TypeAnnotation.cpp
        FunctionSignature.cpp
        Param.cpp
        Block.cpp
        Statement.cpp
        Expression.cpp
        )

set(LIBHADES_AST_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")

add_library(libhades_ast ${LIBHADES_AST_SOURCES})

target_include_directories(libhades_ast
        PUBLIC ${LIBHADES_AST_INCLUDE_DIRS})

target_link_libraries(libhades_ast
        libhades_base
        libhades_core
        )
