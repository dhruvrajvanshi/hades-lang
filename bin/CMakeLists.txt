
message(STATUS "hades include directories ${HADES_INCLUDE_DIRS}")
set(HADES_COMPILER_SOURCES
        hades.cpp)
add_executable(hades ${HADES_COMPILER_SOURCES})
target_include_directories(hades PUBLIC ${HADES_INCLUDE_DIRS} ${LLVM_INCLUDE_DIRS})
target_link_libraries(hades PRIVATE libhades)

set(HADES_TEST_SOURCES run-tests.cpp)
add_executable(run-tests ${HADES_TEST_SOURCES})
target_include_directories(run-tests PUBLIC ${HADES_INCLUDE_DIRS} ${LLVM_INCLUDE_DIRS})
target_link_libraries(run-tests PRIVATE libhades)