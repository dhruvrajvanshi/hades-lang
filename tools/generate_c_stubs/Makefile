HDC=../../hadesboot/build/install/hadesboot/bin/hadesboot

STDLIB_DIR=../../stdlib
RUNTIME_SOURCE=../../runtime.c
LIBC_SOURCES=../../stdlib/libc.c
CC=clang

BUILD_ARTIFACTS=\
	generate_c_stubs.o \
	generate_c_stubs

STDLIB_C_SOURCES=\
	$(shell find $(STDLIB_DIR) -name '*.c') \
	$(shell find $(STDLIB_DIR) -name '*.h')

HADES_SOURCES=\
	$(shell find . -name '*.hds') \
	$(shell find $(STDLIB_DIR) -name '*.hds') \
	$(STDLIB_C_SOURCES)

C_SOURCES=$(shell find . -name '*.c')


generate_c_stubs: $(HADES_SOURCES) $(C_SOURCES)
	CC=$(CC) $(HDC) \
		--main generate_c_stubs.hds \
		--output generate_c_stubs \
		-g \
		--directories . $(STDLIB_DIR) \
		--c-sources $(LIBC_SOURCES) $(C_SOURCES) $(STDLIB_C_SOURCES) \
		--runtime $(RUNTIME_SOURCE) \
		--cflags '-lclang'

clean:
	- rm $(BUILD_ARTIFACTS)

