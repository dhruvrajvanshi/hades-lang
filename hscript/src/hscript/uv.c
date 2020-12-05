#include "stdlib.h"
#include "uv.h"

uv_timer_t* hscript_uv_timer_new() {
    return malloc(sizeof(uv_timer_t));
}

void hscript_uv_timer_set_data(uv_timer_t* timer, void* data) {
    timer->data = data;
}

void* hscript_uv_timer_get_data(uv_timer_t* timer) {
    return timer->data;
}