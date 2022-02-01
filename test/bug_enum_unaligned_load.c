#include <stdio.h>
#include <stdint.h>

#if __APPLE__
    #define SIZE_T_FORMAT "%zu"
#else
    #define SIZE_T_FORMAT "%llu"
#endif
void hscript_put_usize(size_t size, FILE* file) {
    fprintf(file, SIZE_T_FORMAT, size);
}
