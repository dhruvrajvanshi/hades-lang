set(HADES_IR_COMPILER_SOURCES
    hades-ir-compiler.cpp)

add_executable(hades-ir-compiler ${HADES_IR_COMPILER_SOURCES})

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

target_link_libraries(hades-ir-compiler PRIVATE LLVM-10)
