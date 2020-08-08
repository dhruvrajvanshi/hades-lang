
set(LIBHADES_IRGEN_SOURCES
        IRGen.cpp
        IRGenImpl.cpp
        )

set(LIBHADES_IRGEN_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")

add_library(libhades_irgen ${LIBHADES_IRGEN_SOURCES})

target_include_directories(libhades_irgen PUBLIC ${LIBHADES_IRGEN_INCLUDE_DIRS})

target_link_libraries(libhades_irgen
        libhades_base
        libhades_core
        libhades_context
        libhades_ast
        LLVM-10
        )
