# A fixed-size array has its whole-value operations stated by each unit naming it

Date: 2026-10-15 Status: accepted. Revised 2026-10-16: a union is a declaration, as a structure is.

## Context

A fixed-size unpacked array (LRM 7.4.2) has the whole-value operations a structure has: the
equalities, `$bits`, `$countbits`, `$isunknown`, the bit stream both ways, the net folds, whether a
write changed a value. Each is defined element by element (LRM 11.4.5, 6.24.3, 6.7.1).

A structure's operations, and a union's, are methods its declaration states in MIR
([a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md)). An
array had no such body. The C++ backend instantiated its library template for each array type, which
is code compiled for the type. The execution backend handed its prebuilt library a constant
describing the type -- the count and the element -- and the library carried out every operation by
reading it. That is a description of a type interpreted by code compiled before the type existed,
which [a-packed-value-is-its-words](a-packed-value-is-its-words.md) D7 forbids: such code is told
numbers and the addresses of functions the design contains, never a type to interpret. It also left
the two backends doing different work for one MIR, where the execution backend is meant to do what
clang does with the C++ backend's rendering of it.

An array type has no declaration. It is structural (LRM 6.22.1 f): two arrays of the same element
type and range are the same type wherever each is written, and a typedef only names one. A type
written wherever it is used is what a generic instantiated per use answers: rustc monomorphizes a
generic function in each crate that uses it, and clang instantiates a class template's members in
each translation unit that names them.

## Decision

**Each unit states, for every fixed-size array type it names, one body per whole-value operation the
type has, and a use calls that body by the type and the operation.** The unit already stated one
such body per array element type -- its default
([an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md)
D2) -- and that is now one case of the same list. An operation the type does not have has no body,
by the rules a structure's methods follow.

- The body is a loop over the positions, the element's operation applied at each; its count is a
  constant of the body.
- The call takes each value by reference, as a structure's method does. The C++ backend spells it as
  the library template's own member, `a.UnpackedArray<T, 4>::operator==(b)`, which the host compiler
  instantiates where the call is written, and renders none of the bodies, as it renders no element
  default. The execution backend compiles every body into the records handed to whatever asks one
  ([a-type-is-told-to-the-library-as-a-record-both-sides-read](a-type-is-told-to-the-library-as-a-record-both-sides-read.md)
  D4), and fills the type's table with its copy, move, assignment and ending, each taking every
  element's step, which the lowering to LIR states
  ([a-types-steps-are-stated-in-lir](a-types-steps-are-stated-in-lir.md)).
- A tuple's records are generated the same way. Its default is its declaration's method where it is
  a structure, and otherwise a body the unit states from each component's default, stated only where
  every component has one (no declaration holds a wildcard index, LRM 7.8.1).
- What the prebuilt library keeps is what needs no knowledge of the type: reaching an element, a run
  of them and their order by position, building an array from its elements and merging two under an
  ambiguous condition, for each of which the array's constant states a count and the element's
  table, as the dynamic containers' entries are told theirs.
- The C++ library template is the operation's generic definition, as Rust's core states
  `impl<T, U, const N: usize> PartialEq<[U; N]> for [T; N]` once and `std::array` its `operator==`;
  each unit's body is that generic instantiated by Lyra for the backend with no host compiler, as
  rustc monomorphizes.

## Rejected

- **The library carries out each operation from a description of the type.** What D7 of the packed
  value record forbids, and the execution backend then does different work from the C++ backend's
  instantiated templates.
- **The code generator instantiates each operation itself**, as clang instantiates a template's
  body. That would be a third statement of the operation's meaning, in the code generator, which the
  backend contract gives to MIR alone.
- **The C++ backend rendering the unit's bodies** in place of the library template's members. A
  rendered body names constants of its own unit, so two units' definitions of one array type would
  differ in text, which the one-definition rule forbids; and the library's own generic code reaches
  every value's operations as members of its type.
- **A union stated per unit the same way.** It was, first. But the language identifies a union by
  its declaration (LRM 6.22.1 c, d), as it does a structure, and the front end hands over that
  declaration with the type; keyed by its members, two unions declared alike would share what is
  keyed that way, and every unit naming one would state its bodies again. rustc makes a struct, a
  union and an enum one kind of declared type for the same reason.
- **Only the operations a unit uses, instantiated on demand.** A value held in a variable, a net or
  a container reaches its steps and change test through its table, and nearly every type a unit
  names is held; an element is compared and streamed through its container's records whichever unit
  asks.
- **An array's loop per element type with the count an operand**, as rustc compares a slice. The
  table still needs a function of fixed parameters per array type, so the count is spelled per type
  either way.

## Consequences

- A unit states up to sixteen bodies for each fixed-size array type it names. Two units naming one
  type state it twice, as each translation unit instantiates a template.
- The library's whole-value array entries go. Its array type keeps element access, and building an
  array from its elements or merging two, told the count and the element's table.
- A unit's count of distinct constants can fall where a constant an array's body states coincides in
  value with another; a measurement comparing two sizes chooses sizes where it does not.

## Cross-references

- LRM 6.7.1, 6.22.1, 6.24.3, 7.4.2, 11.4.5, 11.4.14
- [a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md) -- the
  same operations for a type with a declaration.
- [a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) -- how an array
  is laid out and reached.
- `../architecture/backend_contract.md` -- a backend chooses spellings, never operations.
