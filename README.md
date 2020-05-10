# Hades
A systems level programming language that compiles to LLVM

- [x] Easy interop with C
- [x] Structs
- [x] Generic types
- [x] Extension Methods
- [] Automatic Refcounting
- []  Closures
- []  Named function arguments
- [] Interfaces
- [] Algebraic data types (enums)
- Editor integration
    - Syntax hilighting
        - [x] VSCode
        - [x] Intellij
    - [] Error highlighting
    - [] Autocompletion
    - [] Debugger support

## Editor support
Git checkout this repo
Basic syntax hilighting is supported
### IntelliJ
Got to `File > Settings > Editor > TextMate bundles`
Add a new bundle from directory `ide/vscode-hades`.
You should get basic syntax hilighting for .hds files.

### VS Code
Plugin isn't published yet. It's in `ide/vscode-hades` Figure it out.
Instructions to come in the future.

## Why?
Because building compilers is fun. The goal is to make a language
that I would like to use instead of Java for writing server and
GUI apps but allows me to drop down to a lower level if I
need to write, for example, a scripting language
interpreter witha a garbage collector.

1. The idiomatic high level application code should be memory
   safe (possibly dropping to runtime checks to do this) if
   not possible at compile time. Refcounting/Optional GC.
   
2. The idiomatic low level code should allow flexibility of
   C/C++ while still being providing higher level safe
   primitives when possible.
   
3. We should have 80% of Rust's safety at compile time while
   and provide the remaining 20% using runtime checks/adress sanizier/
   thread sanitizer in debug builds
   (maybe even production builds if the runtime overhead isn't too
   much).

## What does it look like?
Check the suite directory for a few contrived examples used as an automated test suite.
Proper documentation coming in the future.

