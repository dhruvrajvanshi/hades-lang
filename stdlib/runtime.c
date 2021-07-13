#include <stdlib.h>
#include <stdio.h>

extern void hades_main();
int __hds_termimate();

#ifdef DEBUG
static size_t __hds_allocated;
#endif

int main() {
#ifdef DEBUG
    __hds_allocated = 0;
#endif
    hades_main();
    return __hds_termimate();
}

void* __hds_pointer_add(void* ptr, size_t size) {
    return (void*)((char*)ptr + size);
}

int __hds_termimate() {
#ifdef DEBUG
    if (__hds_allocated != 0) {
        puts("Memory leak detected");
        return 1;
    }
#endif
    return 0;
}


void* __hds_malloc(size_t size) {
#ifdef DEBUG
    __hds_allocated++;
#endif
    return malloc(size);
}

void __hds_free(void* ptr) {
#ifdef DEBUG
    __hds_allocated--;
#endif
    free(ptr);
}

double hds_int_to_double(int value) {
    return (double) value;
}
double hds_size_to_double(size_t value) {
    return (double) value;
}

size_t hds_double_to_size(double value) {
    return (size_t) value;
}

size_t hds_int_to_size(int value) {
    return (size_t) value;
}

#ifdef __HDC_CHKSTK_UNAVAILABLE
// calls to __chkstk are automatically inserted by gcc
// when compiling with mingw, but this function isn't
// found by the linker. This is a stub to make the linker
// happy.
void __chkstk() {}
#endif