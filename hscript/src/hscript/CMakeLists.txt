
add_executable(hscript main.c)
target_link_libraries(hscript PRIVATE hscript_vm)
target_include_directories(hscript PRIVATE ../include ../vm)