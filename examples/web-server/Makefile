HADES_HOME=../../hadesboot/build/install/hades/
HDC=${HADES_HOME}/bin/hades

CC=cc

BUILD_ARTIFACTS=\
	web-server.o \
	web-server \
	test-build

HADES_SOURCES=\
	$(shell find . -name '*.hds') \
	$(shell find $(STDLIB_DIR) -name '*.hds') \
	$(STDLIB_C_SOURCES)

C_SOURCES=$(shell find . -name '*.c')
TEST_SOURCES=$(shell find . -name '*_test.hds')

HADESC_FLAGS=-g \
		--module-path . \
		--c-sources "$(C_SOURCES)"

web-server: $(HADES_SOURCES) $(C_SOURCES) $(STDLIB_C_SOURCES)
	CC=$(CC) HADES_HOME=$(HADES_HOME) $(HDC) build \
		--main main.hds \
		--output web-server \
		$(HADESC_FLAGS)

test: $(HADES_SOURCES) $(C_SOURCES) $(STDLIB_C_SOURCES) $(TEST_SOURCES)
	mkdir -p test-build; \
	for source in $(TEST_SOURCES); \
	do \
		CC=$(CC) $(HDC) \
			--main $$source \
			--output test-build/$$source.bin \
			$(HADESC_FLAGS);	\
		./test-build/$$source.bin; \
	done;


clean:
	- rm $(BUILD_ARTIFACTS)


