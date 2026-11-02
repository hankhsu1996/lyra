# A fixed-size array has its whole-value operations stated by each unit reaching them

Date: 2026-10-15 Status: accepted. Revised 2026-10-16: a union is a declaration, as a structure is.
Revised 2026-11-01: a unit builds an operation where something needs it, found below MIR on finished
LIR, not every operation of every type it names.

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
generic function in each crate that uses it, and clang instantiates a class template's member in
each translation unit that references it.

## Decision

**Each unit builds a body for each whole-value operation of a fixed-size array something in it
needs, and a use calls that body by the type and the operation.** The default of an element type
([an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md)
D2) is one case of the same list. An operation the type does not have has no body, by the rules a
structure's methods follow.

- Need is found where it becomes concrete, which is below MIR, and the body is made in the earliest
  phase that has all it needs. One loop serves a fixed-size array, a tuple, an array's passage
  through the bound-free form and every default. The unit's own bodies are lowered to LIR. A call
  naming an operation declares its function on the first ask. What the records of the library ask
  for is read off the finished LIR by a query of the target's, which names no type of the target,
  through the one decision of the target's lowering that emission also arranges from. Each new
  operation is built as MIR, legalized and verified alone and lowered, which may ask for more, until
  nothing new is asked, as rustc's collector follows calls and vtables to a fixed point before code
  generation, on the layer that plays the role of rustc's MIR. Emission receives complete LIR and
  runs one way.
- The unit's driver owns the loop and names no target: lowering to LIR takes a way to obtain a body,
  the driver is handed the target's collector as rustc's driver is handed its `CodegenBackend`, and
  whoever chooses the target passes it. The set of types, constants and functions only grows while
  it runs, and a chain of bodies that would not end, which only a generator making an ever larger
  type could be, stops the compile.
- MIR holds none of these bodies, and a call names the pair whether or not a body exists yet, so MIR
  checks only that the type can have the operation. What the call hands and takes is checked where
  both sides exist: LIR verification holds every call of a function of the unit to what that
  function takes and answers, and on the C++ path the host compiler holds the call to the library's
  member. No signature is stated apart from the body, since each caller is compiler output and no
  overload has to be chosen before the body exists, which is what makes clang declare an implicit
  member first. The body is built from the unit's types alone and rewritten as rustc runs its passes
  over a shim, one body at a time. A structure's and a union's methods read their declaration and
  stay where the declaration is lowered.
- A record the library keeps -- a type, an element type, a net's folds -- is made whole by the unit
  that makes it, so a value travelling to another unit carries what will be asked of it. What a call
  asks by operation is handed by the calling unit, which no holder keeps
  ([a-type-is-told-to-the-library-as-a-record-both-sides-read](a-type-is-told-to-the-library-as-a-record-both-sides-read.md)
  D4), so a unit never answers for what another unit will do.
- The unit records each operation asked with its function, or with none where the language does not
  give the type one, which fills a record's slot with the library's failing of it. Emission refuses,
  at compile time, a record naming an operation that was never asked for.

- The body is a loop over the positions, the element's operation applied at each; its count is a
  constant of the body.
- The call takes each value by reference, as a structure's method does. The C++ backend spells it as
  the library template's own member, `a.UnpackedArray<T, 4>::operator==(b)`, which the host compiler
  instantiates where the call is written, and renders none of the bodies, as it renders no element
  default. The execution backend compiles each body stated, none of which another unit names, into
  the records handed to whatever asks one
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
- **Every operation of every array type a unit names**, this record's first form. Its reason was
  that a value held in a variable, a net or a container reaches its steps and change test through
  its table, and nearly every type a unit names is held; an element is compared and streamed through
  its container's records whichever unit asks. That took having a table for reaching every slot of
  it. Holding reaches one body and the default; a comparison is handed by the unit making it. What
  held was that holding is a use, which the loop counts.
- **Predicting what the records ask for from MIR**, as a collection at the end of lowering to MIR.
  Which entries are told a type is one backend's realization, over types of the layer below, so a
  prediction in MIR is a second statement of it, held in step by nothing. That the bodies have to
  exist before the unit is legalized is false: every per-body rewrite takes one body at a time, and
  the builders need only the unit's types.
- **The code generator asking for a body as it emits.** LLVM emission would then run on an
  incomplete unit, and a body would reach it unverified. The query reads the same decision with no
  LLVM type in it.
- **An array's loop per element type with the count an operand**, as rustc compares a slice. The
  table still needs a function of fixed parameters per array type, so the count is spelled per type
  either way.

## Consequences

- A unit states two bodies for an array it only holds, and up to sixteen for one it does everything
  with. Two units reaching one type state it twice, as each translation unit instantiates a
  template.
- A body is internal to its module, so one nothing refers to an optimizer can remove.
- `lyra dump mir` shows no such body, since MIR holds none; `lyra dump lir` shows each.
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
