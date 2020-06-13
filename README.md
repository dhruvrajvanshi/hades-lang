# Hades
 [![Hades](https://circleci.com/gh/dhruvrajvanshi/hades-lang.svg?style=shield)](https://app.circleci.com/pipelines/github/dhruvrajvanshi/hades-lang)

A systems level programming language that compiles to LLVM

- [x] Easy interop with C
- [x] Structs
- [x] Generic types
- [x] Extension Methods
- [ ] Automatic Refcounting
- [ ] Closures
- [ ] Named function arguments
- [x] Interfaces
- [ ] Algebraic data types (enums)
- Editor integration
    - Syntax hilighting
        - [x] VSCode
        - [x] Intellij
    - [ ] Error highlighting
    - [ ] Autocompletion
    - [ ] Debugger support

## Editor support
Basic syntax hilighting is supported
### IntelliJ
1. Git checkout this repo.
2. Go to `File > Settings > Editor > TextMate bundles`
3. Add a new bundle from directory `ide/vscode-hades`.
You should now get basic syntax hilighting for .hds files.

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

Hello world
```python
import libc as c;
def main(): Void {
    c.puts(b"Hello world");
}
```

Local variables
```python

def main(): Void {
  val x: *Byte = b"Hello world"; # the type annotation can be omitted
  c.puts(x);
}

```

A bigger example

```swift
// A struct has a packed layout like C
struct Pair[A, B] {
  val first: A;
  val second: B;
}

def main(): Void {
  if true {
    val pair = Pair(1, b"text"); // Type arguments to Pair are inferred
    print_pair_second(pair); // function arguments are passed as value (a copy of pair is sent to print_pair
    let pair_ptr = &pair; // you can take address of local variables and pass them as pointers
    pair.print_second(); // this is an extension function call
  }
}

def print_pair_second[T](pair: Pair[T, *Byte]): Void {
  c.puts(pair.second);
}

// extension methods are defined by having `this` 
// as the first parameter.
def print_second[T](this: *Pair[T, *Byte]): Void {
  c.puts(*this.second); // this.second gives pointer to the second field of the struct. Dereference it using prefix *
}
```




Check the suite directory for a few contrived examples used as an automated test suite.
Proper documentation coming in the future.

