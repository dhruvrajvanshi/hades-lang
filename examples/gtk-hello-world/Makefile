HADES_HOME=../../hadesboot/build/install/hades/
HDC=${HADES_HOME}/bin/hades

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
	CC=$(CC) HADES_HOME=$(HADES_HOME) $(HDC) build \
		--main main.hds \
		--output $(OUTPUT_NAME) \
		-g \
		$(LIBS) \
		--module-path .

clean:
	- rm $(BUILD_ARTIFACTS)


