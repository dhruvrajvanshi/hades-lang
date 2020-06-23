#!/usr/bin/env bash
set -e

./gradlew install

./hadesboot/build/install/hadesboot/bin/hadesboot \
  --main src/run_suite.hds \
  --runtime runtime.c \
  --output run_suite \
  --directories src stdlib \
  --c-sources stdlib/libc.c

./run_suite
