#include "stdint.h"
#include "stdlib.h"
#include "stdio.h"

char* size_to_string(size_t size) {
    char* buffer = (char*) malloc(20);

    sprintf(buffer, "%d", size);

    return buffer;
}