set(LIBHADES_BASE_SOURCES
        Result.cpp
        sys.cpp
        )

set(LIBHADES_BASE_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")

add_library(libhades_base ${LIBHADES_BASE_SOURCES})
target_include_directories(libhades_base PUBLIC ${LIBHADES_BASE_INCLUDE_DIRS})
target_link_libraries(libhades_base LLVM-10)
