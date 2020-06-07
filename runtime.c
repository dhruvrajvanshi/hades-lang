#include <stdlib.h>

extern void hades_main();

int main() {
    hades_main();
    return 0;
}

void* __hds_pointer_add(void* ptr, size_t size) {
    return ptr + size;
}

