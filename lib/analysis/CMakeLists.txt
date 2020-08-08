
set(LIBHADES_ANALYSIS_SOURCES
        TypeResolver.cpp
        TypeResolverImpl.cpp
        )

set(LIBHADES_ANALYSIS_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")

add_library(libhades_analysis ${LIBHADES_ANALYSIS_SOURCES})

target_include_directories(libhades_analysis PUBLIC ${LIBHADES_ANALYSIS_INCLUDE_DIRS})

target_link_libraries(libhades_analysis
        libhades_base
        libhades_core
        libhades_ast
        )
