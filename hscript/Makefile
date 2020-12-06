HDC=../hadesboot/build/install/hadesboot/bin/hadesboot

STDLIB_DIR=../stdlib
RUNTIME_SOURCE=../runtime.c
CC=cc

BUILD_ARTIFACTS=\
	build \
	test-build

STDLIB_C_SOURCES=\
	$(shell find $(STDLIB_DIR) -name '*.c') \
	$(shell find $(STDLIB_DIR) -name '*.h')

HADES_SOURCES=\
	$(shell find src -name '*.hds') \
	$(shell find $(STDLIB_DIR) -name '*.hds') \
	$(STDLIB_C_SOURCES)

C_SOURCES=$(shell find src -name '*.c')
TEST_SOURCES=$(shell find src -name '*_test.hds')

HADESC_FLAGS=-g \
		--directories . $(STDLIB_DIR) src \
		--c-sources $(C_SOURCES) $(STDLIB_C_SOURCES) \
		--cflags '-Ilibuv/include' \
		--runtime $(RUNTIME_SOURCE) \
		-luv

hscript: $(HADES_SOURCES) $(C_SOURCES) $(STDLIB_C_SOURCES)
	CC=$(CC) $(HDC) \
		--main src/main.hds \
		--output ./build/hscript \
		--cflags '-Ilibuv/include' \
		$(HADESC_FLAGS) \

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
	- rm -rf $(BUILD_ARTIFACTS)


