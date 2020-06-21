#!/bin/bash
set -e

./gradlew install
hadesboot/build/install/hadesboot/bin/hadesboot --output hdc --directories stdlib src --runtime runtime.c -g  --main src/main.hds --cflags "-D DEBUG"\
  --c-sources  src/lib/error.c
