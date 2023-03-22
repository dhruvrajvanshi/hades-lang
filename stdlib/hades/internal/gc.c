#define GC_NOT_DLL
#include <gc.h>
#include <stddef.h>

void* __hades_gc_alloc(size_t size) {
    return GC_malloc(size);
}

void* __hades_gc_alloc_bytes(size_t size) {
    return GC_malloc_atomic(size);
}

void __hades_gc_init() {
    GC_init();
}