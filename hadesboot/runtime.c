extern void hades_main();

int main() {
    hades_main();
    return 0;
}

const char* __hdc__bool_to_string(int value) {
    if (value) {
        return "true";
    } else {
        return "false";
    }
}