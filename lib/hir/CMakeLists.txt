
set(LIBHADES_HIR_SOURCES
        Type.cpp
        )

set(LIBHADES_HIR_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")

add_library(libhades_hir ${LIBHADES_HIR_SOURCES})

target_include_directories(libhades_hir PUBLIC ${LIBHADES_HIR_INCLUDE_DIRS})

target_link_libraries(libhades_hir
        libhades_base
        libhades_core
        libhades_ast
        )
