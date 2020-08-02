cmake_minimum_required(VERSION 3.16.3)
project(hades-il)

set(HADES_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include")
set(CMAKE_CXX_STANDARD 20)

add_subdirectory(bin)
add_subdirectory(lib)
include_directories(${HADES_INCLUDE_DIRS})

find_package(LLVM 10 CONFIG REQUIRED)

message(STATUS "Found LLVM_VERSION ${LLVM_VERSION}")

