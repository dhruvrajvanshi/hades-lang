
set(LIBHADES_SOURCES
        ./core/Context.cpp
        ./core/location.cpp)

add_library(libhades ${LIBHADES_SOURCES} ${HADES_INCLUDE_DIRS})
target_link_libraries(libhades LLVM-10)
target_include_directories(libhades PUBLIC ${HADES_INCLUDE_DIRS})
