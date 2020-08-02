set(HADES_IR_COMPILER_SOURCES
        hades.cpp)

add_executable(hades ${HADES_IR_COMPILER_SOURCES})

message(STATUS "hades include directories ${HADES_INCLUDE_DIRS}")

target_include_directories(hades PUBLIC ${HADES_INCLUDE_DIRS} ${LLVM_INCLUDE_DIRS})

target_link_libraries(hades PRIVATE libhades)
