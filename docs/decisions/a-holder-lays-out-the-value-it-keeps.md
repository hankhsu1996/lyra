# A holder lays out the value it keeps, from the type it is told

Date: 2026-10-12 Status: accepted. Supersedes D6 of
[a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md), and the parts of
[a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) that put a tuple's type in its
bytes and hold a tuple in storage of its own; carries out for every holder what D6 of
[a-packed-value-is-its-words](a-packed-value-is-its-words.md) states of a value wider than a word.

## Context

What holds a value for the prebuilt library on the execution backend -- a variable's cell, a net, a
class's property, what a suspending call answers into -- is a class the library compiles once,
before any design exists. Such a holder of an integral value wider than a word, a tuple or a
fixed-size unpacked array kept an object of the library's standing for the value: the value's bytes
in a second allocation, the object holding their address and, for a tuple, the bytes opening with
the type's address. So the value had two addresses, the object's and the bytes', and the holder's
size was the library object's, the same for every type of a domain. A reference named one address
and the library's typed reference the other, and a net of an aggregate had no storage until its
declaration installed a value.

The C++ backend's holder is `Var<T>` holding `T value_`, which the host compiler lays out from `T`.
What the execution backend does with a value after MIR is what clang makes of that rendering, the
one allowed difference being what the prebuilt library causes.

## The test each choice answers to

1. What a type determines about how its values lie is decided where the type is known, wherever the
   value is kept.
2. A value has one address for as long as its holder lives, so a reference, a binding and a force
   name it from the holder's construction on.
3. What is compiled without the type is told the type and acts only on what does not depend on it.
4. End-to-end iteration time is the primary target, so the library is not instantiated per design.

## What the field does

- LLVM's `TrailingObjects` (`llvm/Support/TrailingObjects.h`) lays a class out followed by objects
  whose count is known only at construction, and one helper states their offsets for every user.
  `SmallVector` (`llvm/ADT/SmallVector.h`) keeps where its elements begin (`BeginX`) even while they
  lie inline after it, so every operation reads one pointer whichever way they lie.
- Rust's `Arc<T: ?Sized>` (`library/alloc/src/rcs/arc.rs`, `arcinner_layout_for_value_layout`) is
  the counts followed by `data: T`, its layout the header's extended by the value's. A value whose
  size only its pointer's metadata gives -- a slice's length, a trait object's vtable
  (`library/core/src/ptr/metadata.rs`) -- is a dynamically sized type, kept as a struct's tail. A
  value wider than a word is that with its width as the metadata, and a tuple or an array with its
  type as the metadata.
- Java (JLS 21 section 12.5) and C# (ECMA-334 sections 15.5.6.1, 15.5.6.3) give every field its
  type's default once the object is allocated, before any constructor code, and run the declared
  initializers afterwards as assignments in textual order; a field read before its initializer runs
  reads that default. C++ and Rust make such a read undefined or reject it, so their
  member-initializer shape does not answer a language that defines it.

Neither of the first two keeps a value it can hold inline behind an object of its own. Code generic
over the value is told its size and finds it at a place one rule states. Code compiled before the
value's type is the condition both serve, so the library being prebuilt is no reason to differ.

## Decision

**D1. A holder is the library's part, then the value laid out by its type.** A holder of an integral
value wider than a word, a tuple, a union or a fixed-size unpacked array ends with the value, laid
out as its type says. The code generator lays such a member out at the size the library states for a
holder followed by a value of that size, as it lays out every other member from its type.

**D2. The holder is told the value's width or type where it is built.** A width for an integral
value, the type's constant for a tuple, a union and an array -- the same constant every entry acting
on such a value is told. Every entry acting on a holder after it is built is told nothing of the
values it is handed, since the holder keeps the type.

**D3. A value past the size a value may lie in what holds it is one address of storage the holder
owns,** allocated where the holder is built and taken into afterwards. The bound is the one
[a-value-too-large-for-a-frame-is-held-through-a-box](a-value-too-large-for-a-frame-is-held-through-a-box.md)
states for a frame.

**D4. The holder keeps the address of the value whichever way it lies,** so generated code reads and
writes the value through one pointer at a fixed offset, which the library states, as it reads a
value up to a word at an offset.

**D5. A holder holds a value from the moment it is built: what the value's type builds before
anything writes it,** and every write afterwards, the declaration's initialization included, assigns
into it. A class's property may be read before its initializer runs (LRM 8.7: its value is then
undefined, not unreadable), so its storage holds a valid value from the object's construction, as
Java and C# give a field its default first. Every type builds that value: a singular type its
default (LRM 6.4, Table 6-7), a container empty, an aggregate each part at its own type's; what the
declaration states (LRM 7.2.2) is written by its initialization after it.

**D6. What the library keeps of a value for itself is a value of its own,** allocated once: the
value a variable held when a time slot began, a procedural continuous assignment's level and what it
keeps beneath, a driver's contribution, a history's entries.

**D7. A tuple's bytes state nothing of its type.** It crosses to the library as its bytes followed
by its type, as a fixed-size unpacked array does, and an entry building one is told its type.

## Rejected

- **A library object standing for the value in the holder,** what this replaces: a second address
  for every value, and a holder whose size the type does not decide.
- **Every holder family instantiated per type,** what clang does for the C++ backend's rendering:
  test 4.
- **A tuple's type in its bytes,** which
  [a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) chose because holders and
  container entries stated no type. Holders state it now (D2), and a container is built knowing its
  element's type, which
  [a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) already answered
  for an array; a word per tuple is then a cost with nothing left to buy.
- **A value built by its declaration's first write, the storage holding nothing until then** -- the
  member-initializer shape of C++ and Rust: a property read before its initializer runs would read
  storage holding nothing, which the language defines as a read of an undefined value, so the
  storage must hold one. It also left every whole write asking at run time whether the value existed
  yet, so only the library could carry it out.

## Consequences

- A value has one address from the moment its holder exists; an aggregate net is named before its
  declaration installs a value.
- Every holder's value is read and written where it lies; only an object of the library's own is
  reached through the library.
- No speed or memory figure is recorded here; none was taken.

## Cross-references

- [a-packed-value-is-its-words](a-packed-value-is-its-words.md) -- a value up to the bound lies in
  what holds it.
- [a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) -- an array
  crosses as its bytes followed by its type; D6 superseded here.
- [a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) -- a tuple's layout and its
  generated operations; its table in the bytes superseded here.
- [a-value-is-its-machine-data](a-value-is-its-machine-data.md) -- what the prebuilt library may be
  told of a type.
