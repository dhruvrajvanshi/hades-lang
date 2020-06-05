#include <stdio.h>
#include <stdbool.h>

void print_int(int value) {
    printf("%d\n", value);
}

void print_bool(bool value) {
    if (value) {
        printf("true\n");
    } else {
        printf("false\n");
    }
}
void print_byte_string(char* message) {
    printf("%s", message);
}

void print_size(size_t size) {
    printf("%zu\n", size);
}

