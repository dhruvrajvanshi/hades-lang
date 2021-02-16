#!/usr/bin/env sh
UNAME=$(uname | tr "[:upper:]" "[:lower:]")
NAME="libhadesboot"
if [ "$UNAME" = "darwin" ]; then
  NAME="$NAME.dylib"
else
  NAME="$NAME.so"
fi

clang -shared \
    hadesboot/libhadesboot.cpp \
    $(llvm-config --cflags) \
    $(llvm-config --ldflags) \
    -std=c++17 -o $NAME
