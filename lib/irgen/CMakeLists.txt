
set(LIBHADES_IRGEN_SOURCES
        IRGen.cpp
        )
add_library(libhades_irgen ${LIBHADES_IRGEN_SOURCES})
target_link_libraries(libhades_irgen
        libhades_base
        libhades_core
        libhades_context
        libhades_ast
        LLVM-10
        )
target_include_directories(libhades_irgen PUBLIC ${HADES_BASE_INCLUDE_DIRS} ${HADES_AST_INCLUDE_DIRS})
