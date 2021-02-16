#!/usr/bin/env sh
UNAME=$(uname | tr "[:upper:]" "[:lower:]")
NAME="libhadesboot"
if [ "$UNAME" = "darwin" ]; then
  NAME="$NAME.dylib"
else
  NAME="$NAME.so"
fi

clang \
    -shared -undefined dynamic_lookup \
    hadesboot/libhadesboot.cpp \
    -I/usr/local/opt/llvm/include \
    -std=c++17 -o $NAME
