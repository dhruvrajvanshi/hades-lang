
set(LIBHADES_AST_SOURCES
        SourceFile.cpp
        Identifier.cpp
        Declaration.cpp
        Type.cpp
        FunctionSignature.cpp
        Param.cpp
        Block.cpp
        Statement.cpp
        Expression.cpp
        )
add_library(libhades_ast ${LIBHADES_AST_SOURCES})
target_link_libraries(libhades_ast libhades_base)
target_include_directories(libhades_ast PUBLIC ${HADES_INCLUDE_DIRS})