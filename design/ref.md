Design of the `ref` type constructor
====================================

## Motivation
Right now, the only way to pass arguments by reference is using raw pointers. We don't want this to be a
pervasive thing in the standard library for obvious reasons.

This type is a lightweight and fully safe alternative to pointers for by reference argument passing.
This isn't a full replacement because they have some (seemingly severe) restrictions which allow them
to be safe. My hope is that these would still be useful for most call by reference use cases.

## Proposal
A new type constructor is added to the language, called `ref`. For example, a reference to a u32 will be spelled
as `ref u32`. A mutable ref would be `ref mut u32`.

What makes `ref` safer than pointers is the restrictions that are put on them. This means it isn't as
flexible as raw pointers, or more sophisticated pointer types like lifetime annotated references in Rust.

### Intuition
The way to think about `ref` is that it is a pointer to an object that lives in the current call stack.
It's lifetime is broader than or equal to the current lexical scope. This means it can never be invalid.

### Restrictions

1. `ref T` can't be stored in a `struct`/`sealed type`/`union` or basically any other aggregate type.
   This ensures that aggregate types never need to annotate any lifetime parameters.
2. `ref T` can't be returned from a function. Because `ref` is a pointer to a value living in the current
   call stack, returning it would be like returning a dangling pointer.
3. `ref T` can't be captured by a closure that escapes the current call frame.
   As of now, closures itself have the same restrictions as `ref` because of similar reasons, so they can capture
   refs because it is guaranteed that they would never be returned/stored.
   Once we have a way to store closures in objects, those closures will not be able to capture refs.
   
## Syntax
```scala
// creating a ref
val x = true;

val r: ref Bool = ref x;

// refs can access struct members using dot operator

def f(r: ref SomeStruct) {
  val x: ref Field = r.some_field; // x is itself a ref
  
  // dereference a ref using the deref keyword
  // this is only valid for types that are bitwise copyable. Otherwise, it is a type error.
  val y = deref x; 
}
extension StringExtensions for String {

  // extension receivers can be ref
  def length(ref this): Size {
    return deref this.m_length;
  }
  
  // or mut refs if they need to modify the value
  def append(ref mut this, char: Char): Void {
    /// ...
  }
}

def f(str: String, sref: ref String) {
  str.length(); // values are promoted to refs when calling extensions
  
  sref.length(); // or directly accessed when they're already refs
}

```
   
  
