HDC=../../hadesboot/build/install/hadesboot/bin/hadesboot

STDLIB_DIR=../../stdlib
RUNTIME_SOURCE=../../runtime.c
CC=cc

OUTPUT_NAME=gtk-hello-world

BUILD_ARTIFACTS=\
	${OUTPUT_NAME}.o \
	${OUTPUT_NAME}

STDLIB_C_SOURCES=\
	$(shell find $(STDLIB_DIR) -name '*.c') \
	$(shell find $(STDLIB_DIR) -name '*.h')

HADES_SOURCES=\
	$(shell find . -name '*.hds') \
	$(shell find $(STDLIB_DIR) -name '*.hds') \
	$(STDLIB_C_SOURCES)

C_SOURCES=$(shell find . -name '*.c')
LIBS=$(shell pkg-config --libs gtk+-3.0)

web-server: $(HADES_SOURCES) $(C_SOURCES) $(STDLIB_C_SOURCES)
	CC=$(CC) $(HDC) \
		--main main.hds \
		--output $(OUTPUT_NAME) \
		-g \
		--directories . $(STDLIB_DIR) \
		--c-sources $(C_SOURCES) $(STDLIB_C_SOURCES) \
		$(LIBS) \
		--runtime $(RUNTIME_SOURCE) \

clean:
	- rm $(BUILD_ARTIFACTS)


