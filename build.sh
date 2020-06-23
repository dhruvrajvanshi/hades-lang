#!/bin/bash
set -e

./gradlew install

HADESBOOT_FLAGS="\
  --directories stdlib src bindings \
  --runtime runtime.c \
  --cflags -D DEBUG \
  --c-sources  src/lib/error.c stdlib/libc.c cJSON/cJSON.c \
  -g \
"

# hadesboot/build/install/hadesboot/bin/hadesboot $HADESBOOT_FLAGS --output hdc --main src/main.hds
hadesboot/build/install/hadesboot/bin/hadesboot $HADESBOOT_FLAGS --output hades-server --main src/hades-server.hds
