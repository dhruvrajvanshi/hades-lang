# Hades Intermediate Language

This is a target independent mid level representation that sits between hades source code
and LLVM.

## Motivation
hadesboot compiler is written in Kotlin. It has been immensely benefitial
to write the frontend in a Garbage collected language in terms of
getting things up and running. It makes a lot of sense for a compile
to run in a GC environment.

However, LLVM's C API, which the kotlin compiler can use is
pretty restricted in terms of the API surface.

To make sure we have access to LLVM's full power, it is
better to use the full C++ API.

This is the biggest motivation.

This is also an opportunity to re-think the language interop
story by possibly supporting multiple calling conventions.
It would be nice to have a seamless interop with C++ headers.
This would be useful when we start bootstrapping.


