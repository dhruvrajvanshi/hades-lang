#include <stdint.h>
#include <stdio.h>


typedef struct {
    uint64_t value_1;
    uint64_t value_2;
    uint64_t value_3;
} ThreeRegisters;

ThreeRegisters make_three_registers(uint64_t value_1, uint64_t value_2, uint64_t value_3) {
    ThreeRegisters value = {value_1, value_2, value_3};
    return value;
}

void print_three_registers(ThreeRegisters value) {
    printf("%lu, %lu, %lu\n", value.value_1, value.value_2, value.value_3);
}