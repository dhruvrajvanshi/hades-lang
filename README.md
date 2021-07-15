# Hades
 [![Hades](https://circleci.com/gh/dhruvrajvanshi/hades-lang.svg?style=shield)](https://app.circleci.com/pipelines/github/dhruvrajvanshi/hades-lang)
 [![Linux](https://github.com/dhruvrajvanshi/hades-lang/actions/workflows/linux.yml/badge.svg)](https://github.com/dhruvrajvanshi/hades-lang/actions/workflows/linux.yml)
 [![Windows](https://github.com/dhruvrajvanshi/hades-lang/actions/workflows/windows.yml/badge.svg)](https://github.com/dhruvrajvanshi/hades-lang/actions/workflows/windows.yml)
 
 
A systems level programming language that compiles to LLVM

- [x] Easy interop with C
- [x] Structs
- [x] Generic types
- [x] Extension Methods
- [x] Closures
      - Pure stack allocated closures (i.e. can't be returned from functions and stored in structs right now)
      - Once we have proper destructor semantics, I'll implement heap allocated closures that get cleaned up according to their lifetime.
- [ ] Named function arguments
- [x] Traits
- [x] Associated trait types
- [x] Algebraic data types (enums)
- [x] Windows support
- Editor integration
    - Syntax hilighting
        - [x] VSCode
        - [x] Intellij
    - [ ] Error highlighting
    - [ ] Autocompletion
    - [x] Debugger support:
          - Very basic support for gdb and lldb debug symbols is supported. This allows stepping through code using VS code. No local variables are shown yet.

## Getting started


### Prerequisites
* Python 3 needs to be installed.
* OpenJDK 11 or later needs to be installed (https://adoptopenjdk.net/ has Java builds for windows).
On Linux, you can usually find `openjdk-11` builds in your package manager (`apt/dnf install openjdk-11`)

### Installation
You can install the latest version of hades using this command
```sh
curl https://raw.githubusercontent.com/dhruvrajvanshi/hades-lang/master/install.py | python3 -
```

On windows, you can run this in prowershell
```powershell
# make sure you've installed python 3 beforehand
(Invoke-webrequest -URI "https://raw.githubusercontent.com/dhruvrajvanshi/hades-lang/master/install.py").Content | python -
```

This will install the latest build from the master branch in ~/.hades/versions/latest/hades (%APPDATA%\hades on windows).
Now, restarting your terminal should put `hades` command on your path.

You can install specific releases by appending `---version <TAG>` to the commands, where `<TAG>` can be replaced with any
[tag version](https://github.com/dhruvrajvanshi/hades-lang/tags) from the releases page. `latest` tag is automatically built when something is pushed to master.



## Development
The current compiler lives under the hadesboot directory (self hosting is a long term goal).

Running the test suite is done using gradle.
```
./gradlew test
```

Building the compiler

```
./gradlew install
```
This installs the compiler executable in hadesboot/target directory. Check out examples directory makefiles
to see how a typical hades binary is built.

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


## Hello world
```scala
import libc as c;
def main(): Void {
    c.puts(b"Hello world");
}
```

Local variables
```scala

def main(): Void {
  val x: *Byte = b"Hello world"; // the type annotation can be omitted
  c.puts(x);
}

```

## Some bindings for a C library

![gtk bindings screenshot](images/screenshot.png)

## A bigger example

```scala
// A struct has a packed layout like C
struct Pair[A, B] {
  val first: A;
  val second: B;
}

def main(): Void {
  if true {
    val pair = Pair(1, b"text"); // Type arguments to Pair are inferred
    print_pair_second(pair); // function arguments are passed as value (a copy of pair is sent to print_pair
    val pair_ptr = &pair; // you can take address of local variables and pass them as pointers
    pair.print_second(); // this is an extension function call

    val mut_pair_ptr = &mut pair // taking a mutable pointer
    puts(mut_pair_ptr.second) // calling dot on a pointer dereferences it
    val second_field_ptr = &pair.second // we can get a field offset using &stuctval.field
    mut_pair_ptr.second = b"value" // dereferencing dot operator works for assignments as well
  }
}

def print_pair_second[T](pair: Pair[T, *Byte]): Void {
  c.puts(pair.second);
}

// extension methods can be defined for any type including types from
// different libraries

// some/nested_module.hds
extension PairExtensions[T] for Pair[T, *Byte] {
    // `*this` means this extension takes its receiver by pointer
    // Use `*mut this` to treat receiver pointer as mutable
    // and `this` to take it as value
    def print_second(*this): Void {
      c.puts(this.second); // Unlike this.second on a pointer to a struct is equivalent to this->second in C
    }
}

// in another file

// Importing a module brings extensions exported by that module
// into scope. This isn't transitive i.e. extensions imported by
// nested_module would not be available here. You have to separately
// import those.
import some.nested_module as nested_module;

def main(): Void {
    val x = Pair(true, b"x");
    // because extension method is declared as *this,
    // we have to take pointer to x to pass it to the method.
    (&x).print_second();
}



```

## Traits
Hades has a trait system similar to Rust (We don't have assosicated types yet but they'll come soon). traits allow us to define operations on generic
type parameters. This has the advantage of better error messages than C++ while still
being more powerful than simpler systems like Java interfaces.

```scala

trait Printable[Self] {
  def print(self: Self): Void;
}

// Interfaces are implemented outside the type declaration
// this means you can make builtin types implement new interfaces
implementation Printable[Bool] {
  def print(self: Bool): Void {
    if *this {
      c.puts(b"true");
    } else {
      c.puts(b"false");
    }
  }
}

// Unlike C++, the body of this function can be checked independently
// of call sites. No type errors on expanded templates :)
// The where clause is a requirement on type T and a caller can only pass things that are Printable
def print[T](value: T): Void where Printable[T] {
  Printable[T].print(value);
}

def main(): Void {
  print(true);
  print(10); // type error: Printable[Int] not found
}

```

Implementations can have type parameters and where clauses, making it possible to implement traits
for generic types based on other traits.

```scala
struct Box[T] {
  val value: T;
}

// this declaration says that for all type Ts, Box[T] is printable if T is printable.
// This makes it more powerful that Java/C# interfaces where it's not possible
// to have an interface for Equality/Hashing and have generic containers conform
// to them based on their type parameter.
// Equality and Hashing, Stringification is baked into all objects in Java to get around this problem
// but it doesn't solve it for custom interfaces.
implementation[T] Printable[Box[T]] where Printable[T] {
  def print(self: Box[T]): Void {
     print(b"Box("); // implementation Box[*Byte] is omitted for berevity
     print(self);
     print(b")");
  }
}

def f() {
  print(Box(true)); // works
  print(Box(b"hello")); // works
  print(Box(10)); // Type error because we haven't provided a Printable[Int] implementation
}
```

Although traits directly can't define extension methods yet, (i.e. you have to call `Trait[Type].method(value)`, instead of `value.method()`,
extension definitions can have where clauses, allowing you to wrap trait functions as extension methods.


```scala
extension StringifiableExtensions[T] for T where Stringifiable[T] {
  def to_string(this): *Byte { return Stringifiable[T].print(this); }
}

def f[T](value: T): Void where Stringifiable[T] {
  // now you can call to_string as a method
  value.to_string();
}
```

Directly defining methods inside traits is being considered.

## Sealed types
Sealed types (also known as algebraic data types) allow you to represent types that can be one of a finite set
of cases.
```kotlin

sealed type Optional[T] {
  Some(value: T);
  None;
}


def main(): Void {
   // Sealed types can be pattern matched on.
   // The cases are checked at compile time. If you
   // decide to add a new case, that will have to
   // be handled in existing match statements.
   val ten = when Optional.Some(10) {
      is Some: x -> s.value,
      is None -> 0
   };
   val zero = when Optional.None[Int] {
     is Some: x -> s.value,
     is None -> 0
   };
}
```

## Closures
Hades supports a limited form of closures that are allocated on the stack. This means that
they can't be returned from functions or stored in structs.
Once proper destruction and move semantics are implemented, this restriction can be lifted by
heap allocated closures.
```scala

def main(): Void {
  puts(apply(true, |value| if (value) b"true" else b"false")); // prints "true"
  // closures can have a block body
  // prints "Done"
  puts(apply(true, |value| {
    if value {
      return b"Done";
    } else {
      return b"Not done";
    }
  });
}

def apply[T, U](arg: T, fn: (T) -> U): U {
  return fn(arg);
}
```

## Misc
Check the suite directory for a few contrived examples used as an automated test suite.
There's a gtk-hello-world which is a good representative program.
Proper documentation coming in the future.

