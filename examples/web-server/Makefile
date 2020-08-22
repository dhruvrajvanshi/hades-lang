HDC=../../hadesboot/build/install/hadesboot/bin/hadesboot

STDLIB_DIR=../../stdlib
RUNTIME_SOURCE=../../runtime.c
CC=clang

BUILD_ARTIFACTS=\
	web-server.o \
	web-server

STDLIB_C_SOURCES=\
	$(shell find $(STDLIB_DIR) -name '*.c') \
	$(shell find $(STDLIB_DIR) -name '*.h')

HADES_SOURCES=\
	$(shell find . -name '*.hds') \
	$(shell find $(STDLIB_DIR) -name '*.hds') \
	$(STDLIB_C_SOURCES)

C_SOURCES=$(shell find . -name '*.c')


web-server: $(HADES_SOURCES) $(C_SOURCES) $(STDLIB_C_SOURCES)
	CC=$(CC) $(HDC) \
		--main main.hds \
		--output web-server \
		-g \
		--directories . $(STDLIB_DIR) \
		--c-sources $(C_SOURCES) $(STDLIB_C_SOURCES) \
		--runtime $(RUNTIME_SOURCE) \

clean:
	- rm $(BUILD_ARTIFACTS)


