#if defined(UNIX)
const char* hdc_dirent_name(struct dirent* entry) {
    return entry->d_name;
}

unsigned char hdc_dirent_type(struct dirent* entry)  {
    return entry->d_type;
}

unsigned char hdc_dirent_is_directory(struct dirent* entry) {
    return S_ISDIR(entry->d_type);
}
#endif