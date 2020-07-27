HDC=../../hadesboot/build/install/hadesboot/bin/hadesboot

STDLIB_DIR=../../stdlib
RUNTIME_SOURCE=../../runtime.c
LIBC_SOURCES=../../stdlib/libc.c
CC=clang

generate_c_stubs: Clang.hds generate_c_stubs.hds
	CC=$(CC) $(HDC) --main generate_c_stubs.hds --directories . $(STDLIB_DIR) --output generate_c_stubs \
		--c-sources $(LIBC_SOURCES) --runtime $(RUNTIME_SOURCE) --cflags '-lclang'