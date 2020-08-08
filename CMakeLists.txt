cmake_minimum_required(VERSION 3.16.3)
project(hades-il)

set(CMAKE_CXX_STANDARD 20)

add_subdirectory(bin)
add_subdirectory(lib)

find_package(LLVM 10 CONFIG REQUIRED)

message(STATUS "Found LLVM_VERSION ${LLVM_VERSION}")

