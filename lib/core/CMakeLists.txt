
set(LIBHADES_CORE_SOURCES
        location.cpp
        CommandLineFlags.cpp
        InternedString.cpp
        Interner.cpp
        )
set(LIBHADES_CORE_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")

add_library(libhades_core ${LIBHADES_CORE_SOURCES})

target_link_libraries(libhades_core libhades_base)
target_include_directories(libhades_core
        PUBLIC ${LIBHADES_CORE_INCLUDE_DIRS})



