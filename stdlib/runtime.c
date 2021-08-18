#include <stdlib.h>
#include <stdio.h>

extern void hades_main();

int main(int argc, char** argv) {
    hades_main(argc, argv);
    return 0;
}

void* __hds_pointer_add(void* pointer, size_t offset) {
    return (void*)((char*)pointer + offset);
}

#ifdef __HDC_CHKSTK_UNAVAILABLE
// calls to __chkstk are automatically inserted by gcc
// when compiling with mingw, but this function isn't
// found by the linker. This is a stub to make the linker
// happy.
void __chkstk() {}
#endif