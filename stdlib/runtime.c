#include <stdlib.h>
#include <stdio.h>

extern void hades_main();

int main() {
    hades_main();
}

void* __hds_pointer_add(void* pointer, size_t offset) {
    return pointer + offset;
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