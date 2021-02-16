HDC=../../hadesboot/build/install/hadesboot/bin/hadesboot

STDLIB_DIR=../../stdlib
RUNTIME_SOURCE=../../runtime.c
CC=cc

BUILD_ARTIFACTS=\
	web-server.o \
	web-server \
	test-build

STDLIB_C_SOURCES=\
	$(shell find $(STDLIB_DIR) -name '*.c') \
	$(shell find $(STDLIB_DIR) -name '*.h')

HADES_SOURCES=\
	$(shell find . -name '*.hds') \
	$(shell find $(STDLIB_DIR) -name '*.hds') \
	$(STDLIB_C_SOURCES)

C_SOURCES=$(shell find . -name '*.c')
TEST_SOURCES=$(shell find . -name '*_test.hds')

HADESC_FLAGS=-g \
		--directories . $(STDLIB_DIR) \
		--c-sources $(C_SOURCES) $(STDLIB_C_SOURCES) \
		--runtime $(RUNTIME_SOURCE)

web-server: $(HADES_SOURCES) $(C_SOURCES) $(STDLIB_C_SOURCES)
	CC=$(CC) $(HDC) \
		--main main.hds \
		--output web-server \
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
	- rm $(BUILD_ARTIFACTS)


