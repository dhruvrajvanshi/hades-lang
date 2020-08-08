
set(LIBHADES_LIBS
        libhades_context
        )

set(HADES_COMPILER_SOURCES
        hades.cpp)

add_executable(hades ${HADES_COMPILER_SOURCES})
target_link_libraries(hades PRIVATE ${LIBHADES_LIBS})

set(HADES_TEST_SOURCES run-tests.cpp)
add_executable(run-tests ${HADES_TEST_SOURCES})
target_include_directories(run-tests PRIVATE ${LIBHADES_PARSING_INCLUDE_DIRS} ${LIBHADES_AST_INCLUDE_DIRS})
target_link_libraries(run-tests PRIVATE ${LIBHADES_LIBS})