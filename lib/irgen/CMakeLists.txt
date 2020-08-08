
set(LIBHADES_IRGEN_SOURCES
        IRGen.cpp
        )
add_library(libhades_irgen ${LIBHADES_IRGEN_SOURCES})
target_link_libraries(libhades_irgen libhades_core LLVM-10)
target_include_directories(libhades_irgen PUBLIC ${HADES_INCLUDE_DIRS})
