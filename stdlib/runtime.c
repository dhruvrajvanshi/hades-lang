#define GC_NOT_DLL
#include <gc.h>
#include <stdlib.h>
#include <stdio.h>

extern void hades_main();

int main(int argc, char** argv) {
    GC_init();
    hades_main(argc, argv);
    return 0;
}

#ifdef __HDC_CHKSTK_UNAVAILABLE
// calls to __chkstk are automatically inserted by gcc
// when compiling with mingw, but this function isn't
// found by the linker. This is a stub to make the linker
// happy.
void __chkstk() {}
#endif