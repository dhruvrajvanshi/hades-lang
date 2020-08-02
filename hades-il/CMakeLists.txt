cmake_minimum_required(VERSION 3.16.3)
project(hades-il)

add_subdirectory(bin)
add_subdirectory(lib)
include_directories(include)

find_package(LLVM 10 CONFIG REQUIRED)

message(STATUS "Found LLVM_VERSION ${LLVM_VERSION}")

include_directories(${LLVM_INCLUDE_DIRS})
