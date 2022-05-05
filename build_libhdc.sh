#!/bin/sh
# Script to test if libhdc builds correctly
# This is only for CI. Normally, hades compiler will
# invoke the platform's C compiler and link this
# in automatically from source.

set -e

gcc -Wall -Werror -O2 -c -std=c11 stdlib/libhdc.c -Istdlib -g
