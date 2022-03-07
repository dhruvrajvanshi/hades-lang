#include <stdio.h>
#include <stdint.h>

void hscript_put_usize(size_t size, FILE* file) {
    fprintf(file, "%zu", size);
}
