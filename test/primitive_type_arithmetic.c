#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

void pta_print_u8(uint8_t value) {
    printf("%hhu", value);
}

void pta_print_i8(int8_t value) {
    printf("%hhi", value);
}

void pta_print_i16(int16_t value) {
    printf("%hi", value);
}

void pta_print_u16(uint16_t value) {
    printf("%hu", value);
}

void pta_print_u32(uint32_t value) {
    printf("%u", value);
}

void pta_print_i32(int32_t value) {
    printf("%i", value);
}

void pta_print_i64(int64_t value) {
    printf("%li", value);
}

void pta_print_u64(uint64_t value) {
    printf("%lu", value);
}