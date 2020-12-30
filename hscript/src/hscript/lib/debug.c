#include "stdint.h"
#include "stdio.h"

void hscript_lib_dump_size(FILE* file, size_t size) {
    fprintf(file, "%lu", size);
}
void hscript_lib_dump_ptr(FILE* file, void* ptr) {
    fprintf(file, "%x", ptr);
}

void hscript_lib_dump_f64(FILE* file, double value) {
    fprintf(file, "%f", value);
}