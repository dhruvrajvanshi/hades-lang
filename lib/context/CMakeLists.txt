set(LIBHADES_CONTEXT_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")

set(LIBHADES_CONTEXT_IMPL_SOURCES
        ContextImpl.cpp
        )
add_library(libhades_context_impl ${LIBHADES_CONTEXT_IMPL_SOURCES})
target_link_libraries(libhades_context_impl
        libhades_base
        libhades_ast
        libhades_parsing
        libhades_irgen
        )
target_include_directories(libhades_context_impl
        PRIVATE
        ${LIBHADES_CONTEXT_INCLUDE_DIRS})

set(LIBHADES_CONTEXT_SOURCES
        Context.cpp
        )
add_library(libhades_context ${LIBHADES_CONTEXT_SOURCES})
target_link_libraries(libhades_context libhades_context_impl)
target_include_directories(libhades_context
        PUBLIC
        ${LIBHADES_CONTEXT_INCLUDE_DIRS}
)

