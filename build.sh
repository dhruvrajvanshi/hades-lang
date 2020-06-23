#!/bin/bash
set -e

./gradlew install

HADESBOOT_FLAGS="\
  --directories stdlib src \
  --runtime runtime.c \
  --cflags -D DEBUG \
  --c-sources  src/lib/error.c stdlib/libc.c \
  -g \
"

# hadesboot/build/install/hadesboot/bin/hadesboot $HADESBOOT_FLAGS --output hdc --main src/main.hds
hadesboot/build/install/hadesboot/bin/hadesboot $HADESBOOT_FLAGS --output hades-server --main src/hades-server.hds
