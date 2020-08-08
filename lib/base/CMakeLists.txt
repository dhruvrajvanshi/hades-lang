set(LIBHADES_BASE_SOURCES
        Result.cpp
        sys.cpp
        )

add_library(libhades_base ${LIBHADES_BASE_SOURCES})
target_include_directories(libhades_base PUBLIC ${HADES_INCLUDE_DIRS})
target_link_libraries(libhades_base LLVM-10)
