#!/usr/bin/env bash
set -e

./gradlew :hadesboot:installDist

HADESBOOT="./hadesboot/build/install/hadesboot/bin/hadesboot"

HDFLAGS="--runtime runtime.c --directories stdlib src"

TEST_FILES=(
  src/lib/string_test
)
DIRECTORIES=(
  src/lib
)
rm -rf test_build

for directory in $DIRECTORIES
do
  mkdir -p test_build/$directory
done

for file in $TEST_FILES
do
  $HADESBOOT $HDFLAGS --main $file.hds --output test_build/$file --cflags "-D DEBUG"
  ./test_build/$file
done

