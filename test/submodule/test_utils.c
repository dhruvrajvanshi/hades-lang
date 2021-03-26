#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

void print_int(int value) {
    printf("%d\n", value);
}

void print_u32(uint32_t value) {
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
const char* bool_to_string(bool value) {
    if (value) {
        return "true";
    } else {
        return "false";
    }
}

typedef struct {
    int x;
    int y;
    int z;
} Point3d;

void abi_test_print_3d_point(Point3d p) {
    printf("%d, %d, %d\n", p.x, p.y, p.z);
}

Point3d abi_test_make_3d_point(int x, int y, int z) {
    Point3d point;
    point.x = x;
    point.y = y;
    point.z = z;
    return point;
}
