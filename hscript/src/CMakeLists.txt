add_subdirectory(vm)

add_executable(hscript main.c)
target_include_directories(hscript PRIVATE ./include)
target_link_libraries(hscript PRIVATE hscript_vm)