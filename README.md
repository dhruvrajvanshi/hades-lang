# Hades
 [![Hades](https://circleci.com/gh/dhruvrajvanshi/hades-lang.svg?style=shield)](https://app.circleci.com/pipelines/github/dhruvrajvanshi/hades-lang)

A systems level programming language that compiles to LLVM

- [x] Easy interop with C
- [x] Structs
- [x] Generic types
- [x] Extension Methods
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
I think there's a lot of room for experimentation in the systems languages space.
This language aims to sit between C and Rust, hopefully leaning
towards Rust in terms of safety and towards C in terms of simplicity.

Takes inspiration from following languages:
- C++
- Rust
- Haskell

And a host of experimental and niche languages like Zig, Lobster, C3, and Odin.

Here's a short list of things which I think Hades aims to improve over its influences

### C++ 
* Error messages: C++, due to the nature of templates, is very hard to produce good error messages for.
C++ templates bodies are checked per use site. i.e. Template bodies don't show type errors. In fact it depends
on this behaviour for overload resolution (SFINAE).

Hades has constrained generics in the form of interfaces (similar to Rust/Haskell in this regard).

### Rust
* Ergonomics. I think it's worth sacrifising some performance to improve ergonomics. Hades doesn't aim
  to provide the memory and aliasing safety guarantees that Rust provides. If we can enforce some of that with
  runtime checks while being more ergonomic than lifetime annotations, that would be nice.



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

Interfaces
```swift

interface Printable {
  // interfaces can refer to the type they
  // are implemented for using the This type
  def print(this: *This): Void;
}

// Interfaces are implemented outside the type declaration
// this means you can make builtin types implement new interfaces
implement Printable for Bool {
  def print(this: *Bool): Void {
    if *this {
      c.puts(b"true");
    } else {
      c.puts(b"false");
    }
  }
}

def main(): Void {
  val boolean = true;
  val pointer_to_boolean = &boolean;
  pointer_to_boolean.print(); // prints true
}

```



Check the suite directory for a few contrived examples used as an automated test suite.
Proper documentation coming in the future.

