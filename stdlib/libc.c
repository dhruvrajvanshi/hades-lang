#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <errno.h>

const char* hdc_dirent_name(struct dirent* entry) {
    return entry->d_name;
}

unsigned char hdc_dirent_type(struct dirent* entry)  {
    return entry->d_type;
}

unsigned char hdc_dirent_is_directory(struct dirent* entry) {
    return S_ISDIR(entry->d_type);
}

int hdc_get_errno() {
    return errno;
}
void hdc_set_errno(int value) {
    errno = value;
}

FILE* hdc_stdout() {
    return stdout;
}

FILE* hdc_stdin() {
    return stdin;
}

FILE* hdc_stderr() {
    return stderr;
}

