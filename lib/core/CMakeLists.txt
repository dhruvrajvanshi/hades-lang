
set(LIBHADES_CORE_SOURCES
        location.cpp
        CommandLineFlags.cpp
        InternedString.cpp
        )

add_library(libhades_core ${LIBHADES_CORE_SOURCES} ${HADES_INCLUDE_DIRS})
target_link_libraries(libhades_core libhades_base)
target_include_directories(libhades_core PUBLIC ${HADES_INCLUDE_DIRS})


