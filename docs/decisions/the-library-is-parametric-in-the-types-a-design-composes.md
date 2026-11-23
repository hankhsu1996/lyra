# The library is parametric in the types a design composes

Status: Accepted, 2026-11-19.

## Requirement

Code compiled once, before any design exists, does to a value of a type a design composes -- an
array of any kind, a structure, a union -- only what the operations handed to it for that type do.
Algorithms over such types had moved one at a time from the library into bodies MIR states for each
type -- a container's methods, a sort's order, a memory task's load and dump, a DPI-C open array's
image -- each argued on its own, with nothing saying where the moves end. A rule stated as "no
template in the library" answers nothing, since the C++ backend's library is header templates by
design, the execution backend's has none, and either could still walk a type through its
description.

## Choice

- **The library -- compiled once for the execution backend, or instantiated by emitted C++ -- never
  chooses what to do by a type's kind, and never walks what a type is composed of.** A loop over a
  fixed-size array's elements or a tuple's components is a body MIR states for the type.
- **A facility the library declares for one kind knows that kind and where its elements lie, and is
  parametric in its arguments**, as Rust's `Vec<T>` knows it is a `Vec`; it reaches an element's
  value only through the element type's items.
- **The library reaches a composed value three ways only** -- a service that is the same whatever
  the type (storage, files, the solver, the scheduler, a container's core); an item of a library
  trait the type implements, as `dyn Trait` reaches a type through its vtable and nothing of its
  fields; and a description read as data by a tool reporting the design (VPI, LRM 37; a value change
  dump, LRM 21.7), as a debugger reads debug information.
- **The values the library defines itself are no composed type** -- an integral's bits, a string's
  text, a class handle, a `chandle`, the index kinds an associative array orders (LRM 7.8).

This is parametricity. A function generic in a type cannot inspect it, so what it does with a value
of that type is what the operations handed to it do (Wadler, "Theorems for free!", 1989).

## Alternatives turned down

- **No template body in the library.** It fails the requirement by not reaching it, since a library
  without templates can still choose by kind. The failure rests on templates being how C++ ships a
  library, which says nothing about where an algorithm lives.
- **A library that knows no kind at all.** It fails a container whose storage is a core declared per
  kind, the library operating on its own values, and a tool reporting the design, which needs each
  variable described.
- **Each place that chooses by kind filed only as work.** It fails stopping a new place from
  appearing beside the old ones while the work is undone; only a check does that.

## Consequences across layers

- A library function that needs a composed type's structure is a body MIR states for the type, or an
  item of a trait it implements, never a branch in the library.
- A032 of `tools/policy/check_architecture.py` refuses a choice by kind across the library, and
  lists the places that still make one as a queue that only shrinks. A walk of a type's composition
  is not checked, since telling it from a facility reaching one element is whether the code loops.
- The traits and the cores are [`mir.md`](../architecture/mir.md)'s, and the record a type is told
  to the library by is [`runtime_distribution.md`](../architecture/runtime_distribution.md)'s.
