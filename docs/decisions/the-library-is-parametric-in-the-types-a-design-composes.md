# The library is parametric in the types a design composes

Date: 2026-11-19 Status: accepted.

## Context

Algorithms over the types a design composes -- an array of any kind, a structure, a union -- have
moved, one at a time, from the library into bodies MIR states for each type: a container's methods,
the steps of its value's life, a sort's order, a memory task's load and dump, a DPI-C open array's
image, the walk of a random variable
([a-containers-operations-are-built-for-its-type-over-a-core](a-containers-operations-are-built-for-its-type-over-a-core.md),
[a-type-implements-the-librarys-traits](a-type-implements-the-librarys-traits.md)). Each move was
argued on its own, and nothing said where the moves end: what the library may still know of such a
type, and what it may not. A rule stated as "no template in the library" would answer nothing, since
the C++ backend's library is header templates by design and the execution backend's has none at all,
and either could still walk a type through its description.

## Decision

**D1. The library -- compiled once for the execution backend, or instantiated by emitted C++ -- is
parametric in the types a design composes.** Code the library has for every type never chooses what
to do by a type's kind, and the library never walks what a type is composed of: a loop over a
fixed-size array's elements or a tuple's components is a body MIR states for the type. A facility
the library declares for one kind -- a container's core, the class a fixed-size array or a tuple is
instantiated as on the C++ backend, the description the execution backend's library reads one of
them by, and an entry MIR calls for that kind -- knows its own kind and where each of its elements
lies, as Rust's `Vec<T>` knows it is a `Vec`, and is parametric in its arguments: it reaches an
element's value only through the element type's items. The library reaches a value of a composed
type in three ways only:

- a service that is the same whatever the type: storage, files, the solver, the scheduler, a
  container's core;
- an item of a library trait the type implements, which the type's record holds or the class its
  templates are instantiated at names -- the type's own body, as Rust's `dyn Trait` reaches a type
  through its vtable, whose entries are the destructor, the size, the alignment and the trait's
  methods (`compiler/rustc_middle/src/ty/vtable.rs`, `VtblEntry`) and nothing of the type's fields;
- a description of the type read as data, for a tool reporting the design rather than for work the
  library does on a value: the VPI object model (LRM 37) and a value change dump (LRM 21.7), as a
  debugger reads a program's types from its debug information and not from its code.

Anything else the library would do by a type's kind or composition is a body MIR states for that
type. This is parametricity: a function generic in a type cannot inspect it, so what it does with a
value of that type is only what the operations handed to it do (Wadler, "Theorems for free!", 1989,
recalled rather than reopened here).

**D2. The values the library defines itself are no type a design composes.** It operates on an
integral's bits, holds a string's text, compares class handles, `chandle`s and the index kinds an
associative array's core orders itself (LRM 7.8), so telling these apart and reading an integral's
shape is the library knowing its own types, as Rust's standard library knows `u32` and `str`.

**D3. The choice by kind is checked, and what still makes one is listed.**
`tools/policy/check_architecture.py` A032 refuses, across the library's sources and headers, a case
on a value type's kind, a comparison with one, a question its kind answers -- called, or named as a
concept -- the kind a facility declares for such a question, and an ask of a kind's own description.
The places that still do are listed there with what each does, and the list only shrinks: an entry
no longer read there fails the check until it is dropped, so the list is the queue of what is left.
The check names the forms it knows; a new form a later change finds joins it. A walk of a type's
composition is not checked: what tells it from a facility reaching one element of its own kind is
whether the code loops, which no text pattern sees, so the walks that remain are tracked as work.

## Rejected

- **No template body in the library.** Templates are how C++ ships a library and say nothing about
  where an algorithm lives; the execution backend's library has no template and could still choose
  by a type's kind.
- **The library knows no kind at all.** A container's storage is a core the library declares per
  kind (D1), and it operates on its own values (D2); a tool reporting the design needs each variable
  described.
- **Each violation as an issue alone.** An issue states the work; only the check stops a new place
  from appearing beside the old ones while the work is undone.

## Consequences

- A library function that needs a composed type's structure is a body MIR states for the type, or an
  item of a trait the type implements, never a branch in the library.
- The queue of choices by kind is A032's list, each entry with what it does. The walks of a
  fixed-size array's or a tuple's composition that remain, on both backends, are a work item of
  their own, since no check finds them.

## Cross-references

- LRM 7.8, 21.7, 37
- [a-containers-operations-are-built-for-its-type-over-a-core](a-containers-operations-are-built-for-its-type-over-a-core.md)
  -- the bodies the moved algorithms became, and each kind's core.
- [a-type-implements-the-librarys-traits](a-type-implements-the-librarys-traits.md) -- the traits
  through which the library reaches a type.
- [a-type-is-told-to-the-library-as-a-record-both-sides-read](a-type-is-told-to-the-library-as-a-record-both-sides-read.md)
  -- the record a library trait's items are reached through.
