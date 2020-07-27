HDC=../../hadesboot/build/install/hadesboot/bin/hadesboot

STDLIB_DIR=../../stdlib
RUNTIME_SOURCE=../../runtime.c
LIBC_SOURCES=../../stdlib/libc.c
CC=clang

BUILD_ARTIFACTS=\
	generate_c_stubs.o \
	generate_c_stubs

HADES_SOURCES=$(wildcard **/*.hds)
C_SOURCES=$(wildcard **/*.c)
STDLIB_C_SOURCES=$(wildcard $(STDLIB_DIR)/**/*.c)

generate_c_stubs: $(HADES_SOURCES) $(C_SOURCES)
	CC=$(CC) $(HDC) -g --main generate_c_stubs.hds --directories . $(STDLIB_DIR) --output generate_c_stubs \
		--c-sources $(LIBC_SOURCES) $(C_SOURCES) $(STDLIB_C_SOURCES) --runtime $(RUNTIME_SOURCE) --cflags '-lclang'

clean:
	- rm $(BUILD_ARTIFACTS)

