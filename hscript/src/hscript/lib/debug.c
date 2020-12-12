#include "stdint.h"
#include "stdio.h"

void hscript_lib_dump_size(FILE* file, const char* tag, size_t size) {
    fprintf(file, "%s: %lu\n", tag, size);
}
void hscript_lib_dump_ptr(FILE* file, const char* tag, void* ptr) {
    fprintf(file, "%s: %x\n", tag, ptr);
}