#include "stdint.h"
#include "stdio.h"

void example_debug_dump_size(size_t size) {
    fprintf(stderr, "%lu\n", size);
}