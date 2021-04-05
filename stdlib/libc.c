#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <errno.h>

void hdc_set_errno(int value) {
    errno = value;
}


FILE* hdc_get_stdout() {
    return stdout;
}

FILE* hdc_get_stderr() {
    return stderr;
}


int hdc_errno() {
    return errno;
}