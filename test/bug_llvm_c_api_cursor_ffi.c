#include "stdio.h"
typedef struct {
  int kind;
  int xdata;
  void* data[3];
} Cursor;

Cursor make_cursor(int kind, int data) {
    Cursor cursor;
    cursor.kind = kind;
    cursor.xdata = data;
    return cursor;
}


void print_cursor(Cursor cursor) {
    printf("%d, %d\n", cursor.kind, cursor.xdata);
}

int get_cursor_kind(Cursor cursor) {
    return cursor.kind;
}

void print_cursor_kind(int kind) {
    printf("%d\n", kind);
}