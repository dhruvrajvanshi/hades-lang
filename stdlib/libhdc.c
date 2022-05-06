#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <memory.h>

#define HDC_NULLABLE_PTR(T) T*
#define HDC_NULLABLE_CONST_PTR(T) const T*
#define CStr = const char*

extern FILE* _hdc_get_stderr() {
    return stderr;
}

extern FILE* _hdc_get_stdout() {
    return stdout;
}

extern FILE* _hdc_get_stdin() {
    return stdin;
}

extern void _hdc_panic(const char* message) {
    fprintf(stderr, "PANIC: %s(%d):%s\n", __FILE__, __LINE__, message);
    abort();
}

extern HDC_NULLABLE_PTR(FILE) _hdc_file_open(const char* filename, const char* mode) {
    return fopen(filename, mode);
}

extern void _hdc_file_put_cstr(FILE* stream, const char* cstr) {
    fputs(cstr, stream);
}

#define DEFINE_FILE_PUT(name, ctype, formatstr)\
    extern void _hdc_file_put_##name(FILE* stream, ctype value) {\
        fprintf(stream, formatstr, value);\
    }

DEFINE_FILE_PUT(usize, size_t, "%lu")
DEFINE_FILE_PUT(u8, uint8_t, "%u")
DEFINE_FILE_PUT(i8, int8_t, "%d")
DEFINE_FILE_PUT(u16, uint16_t, "%u")
DEFINE_FILE_PUT(i16, int16_t, "%d")
DEFINE_FILE_PUT(u32, uint32_t, "%u")
DEFINE_FILE_PUT(i32, int32_t, "%d")
DEFINE_FILE_PUT(u64, uint32_t, "%u")
DEFINE_FILE_PUT(i64, int32_t, "%d")
DEFINE_FILE_PUT(f32, float, "%f")
DEFINE_FILE_PUT(f64, double, "%f")
DEFINE_FILE_PUT(void_ptr, void*, "%x")

#undef DEFINE_FILE_PUT

void _hdc_memset(
    void* dest,
    int value,
    size_t size
) {
    memset(dest, value, size);
}

