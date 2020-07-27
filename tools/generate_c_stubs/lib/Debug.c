#include <stdio.h>

#include <execinfo.h>
void SG_Debug_dump_int(int value) {
    fprintf(stderr, "%d", value);
}

void SG_Debug_dump_string(const char* str) {
    fprintf(stderr, "%s", str);
}
void SG_Debug_dump_newline() {
    fprintf(stderr, "\n");
}



#define MAX_STACK_LEVELS 50

// helper-function to print the current stack trace
void SG_Debug_dump_stack_trace()
{
  void *buffer[MAX_STACK_LEVELS];
  int levels = backtrace(buffer, MAX_STACK_LEVELS);

  // print to stderr (fd = 2), and remove this function from the trace
  backtrace_symbols_fd(buffer + 1, levels - 1, 2);

}