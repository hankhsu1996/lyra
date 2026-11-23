# Lyra links no C++ front end, and its foreign boundary is C

Status: Accepted, 2026-11-22.

## Requirement

The edit-compile-run loop is the primary optimization target
([`north_star.md`](../architecture/north_star.md) invariant 1), so nothing is paid on every unit's
compile that no design needs. Code SystemVerilog calls out to, and code that calls into it, crosses
the boundary the standard defines, which is C (LRM 35, DPI-C), and the only C++ a program Lyra
builds meets is the runtime library Lyra ships and is itself built with.

## Choice

- **Lyra links no C++ front end, and generated code matches no C++ compiler's ABI.** Nothing the
  execution backend emits has to agree with a virtual table, type information or a constructor
  variant of a C++ compiler's.
- **Every fact generated code needs of the runtime is a fact of the library Lyra ships, read when
  Lyra is built** -- the size of the part every object starts with, where each member of a structure
  generated code reaches lies, the entries that begin and end that part -- and the runtime publishes
  what generated code calls as a C ABI.
- **The foreign boundary a design publishes and calls is C**, as LRM 35 states it.

## Alternatives turned down

- **Linking clang in process, as Swift and Carbon do**, to lay out generated classes or read C++
  headers. It fails paying nothing per unit that no design needs, since headers would be parsed on
  every compile. The failure rests on Lyra importing no C++ it did not write; Swift and Carbon link
  clang because they import arbitrary user headers whose layout only a C++ compiler knows, and Swift
  still needed a forked clang entry to emit tables for bodies it defines.
- **Generated code posing as the library's C++ classes**, a table laid out by guessing the
  compiler's layout or found by its mangled name. It fails an entry and its call agreeing by
  construction. The failure rests on that layout being one no header publishes, so a reordered
  declaration moves a call to the wrong function with nothing to say so.
- **A foreign boundary in C++**, letting DPI reach C++ classes. It fails the standard's boundary and
  brings back the C++ front end above.

## Consequences across layers

- The C++ backend's emitted classes still follow the C++ ABI where they derive from the library's
  scope and object, since a C++ compiler compiles them; the execution backend reads the same
  declarations by `offsetof` instead
  ([`runtime_distribution.md`](../architecture/runtime_distribution.md)).
- What a design publishes to foreign code and how foreign code is linked is
  [`foreign_interface.md`](../architecture/foreign_interface.md)'s.
