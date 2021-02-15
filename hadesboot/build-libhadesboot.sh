clang \
    -shared -undefined dynamic_lookup \
    libhadesboot.cpp \
    -I/usr/local/opt/llvm/include \
    -std=c++17 -o libhadesboot.dylib