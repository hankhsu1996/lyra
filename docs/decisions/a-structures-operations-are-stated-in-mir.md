# A structure's whole-value operations are methods its declaration states in MIR

Date: 2026-09-27 Status: accepted.

## Context

The language defines operations on an unpacked structure as a whole: `==` and `!=` (LRM 11.2.2,
11.4.5), `===` and `!==` (11.4.5), `$bits` (20.6.2), `$countbits` and `$isunknown` (20.9), the bit
stream in both directions (6.24.3, 11.4.14), and resolving a net of the type (6.6, 6.7.1). Whether a
write changed a variable is decided the same way (4.3, 9.4.2). Every one is defined member by
member. The runtime performs them too, on values it holds: a container's elements, a variable's
change test, a net's drivers.

Each backend defined them itself: the C++ value library composed them in `Tuple<Ts...>`, and the
execution backend's code generator composed them once per type. What an operation means for a type
was therefore stated below MIR, twice, and a backend was deciding the meaning of `==` -- which the
backend contract gives to MIR alone.

A structure is identified by its declaration (LRM 6.22.1): two declarations with the same members
are two types, an anonymous one matches only within its own declaration statement, and a structure
shared across instances or units is declared in a package or the compilation-unit scope, which every
unit sharing it already depends on (LRM 6.22).

clang states a defaulted `operator==` in Sema, as an AST body its synthesizer builds
(`clang/lib/Sema/SemaDeclCXX.cpp`, `DefaultedComparisonSynthesizer`), and CodeGen compiles it like
any function; an implicit destructor's body is empty in Sema and CodeGen emits the member
destruction (`clang/lib/CodeGen/CGClass.cpp`, `EnterDtorCleanups`). Rust's `derive` expands at the
declaration and the impl is compiled in the defining crate; Swift synthesizes `Equatable` in Sema
and emits a type's value witnesses in the defining module (both recalled, not read).

## Decision

**A structure type is its declaration -- in the declaring unit its place in that unit's struct
registry, in any other unit the declaring unit and the declaration's name there -- and the
declaration states one method per whole-value operation the type has.** The operations are the
questions every value type answers -- the equality operators, and the entries the runtime library
asks of every value it holds -- so a structure's method answers one of them the way a Rust type's
`impl PartialEq` answers `eq`, and is identified by the structure and the operation it answers. Each
body applies the same operation to every member -- through the member type's own method where the
member is a structure -- and combines the answers. Where the operation is asked of a value, the
method's first parameter is that value, its receiver; where it is asked of the type, as building one
from a stream is, there is none. Either way it takes the parameters the operation takes of any
value, so a call to a structure's method and a call to the library's entry for the same question
differ only in the callee: the lowering picks the method where the value, or for a question of the
type the result, is a structure the source declared, which is the overload resolution clang does in
Sema before `CXXOperatorCallExpr` names the function. The program's `x == y` on a structure is that
call, and so is every whole-value question a structure's own body asks of a member.

- **The declaration brings the methods, so they exist whether or not the unit uses them.** A
  structure's typedef is interned with the unit's declarations for that reason, in whatever scope
  outside a body it stands in. Another unit naming the type calls them by the pair that identifies
  them, across the dependency it already has, and never states them again.
- **An operation the type does not have has no method.** A real member leaves no case equality (LRM
  11.4.5), a real or a chandle no bit stream (6.24.3), a stream is built only where the type fixes
  its width, and only a type valid for a net (6.7.1) resolves.
- **The questions the runtime asks as predicates are entries of their own**: whether two values are
  the same bits and whether one holds an unknown bit. So is each of the net's three truth tables,
  for the reason a net's installation is one entry per resolution. `!=` and `$isunknown`'s bit are
  methods too, because the library asks them of every value it holds.
- **A backend compiles those methods as it compiles any declared body**, lays the structure out, and
  derives copying, moving, ending and assigning member by member. The C++ backend spells the
  structure as a type of the declaring unit whose members are its methods, each named as the library
  asks every value -- the operator, or the entry's own identifier -- and defined once, in that unit.
  The execution backend's runtime was compiled before the type existed and holds values of it
  erased, so the declaring unit also hands it a table whose operation slots are those methods, one
  table in the program for the type.

## Rejected

- **Each backend composes the operations.** Two definitions below MIR of what the language states
  once, and a code generator deciding the meaning of an operator.
- **MIR states only what the program writes, and the runtime's own needs stay per backend.** A
  container's `==` over structures and the program's `==` would still be two definitions.
- **A description of the structure the runtime walks.** Rejected by the layout decision: a second
  realization of every operation, interpreted at run time.
- **Every unit naming a structure states its operations again**, keyed by the structure's member
  types. It needs no declaration to own them, which is why it was first built, but it rebuilds the
  same bodies in every unit naming the type -- measured at about 9.4 ms of build and 1,400 lines of
  LLVM IR per structure type per unit -- and it keys by members what the language keys by
  declaration, so two structures whose members are held alike share whatever is keyed that way.
- **The C++ type named by a hash of the structure's content, every naming unit emitting identical
  inline copies**, which is how C++ keeps a template instantiated in many translation units one
  definition. It rests on the same missing declaration and rebuilds the same work per unit.
- **A table carried in every C++ value**, reached by the library's `Tuple<Ts...>`, because that type
  is keyed by the C++ component types and cannot tell a `bit [3:0]` member from a `logic [3:0]` one.
  Once the type is the declaration, the C++ type names the structure exactly, so the library reaches
  its operations through the type as it does any other value's, with nothing in the value.
- **The table beside the value, as Swift passes a witness table and Rust a trait object's vtable**
  (recalled, not read). A container's entries take elements of any domain, so no signature can grow
  a table operand for one domain; the layout decision rejected it on the same ground.
- **One resolving function taking the truth table as an operand.** The table is part of what is
  asked, and the builtins already name each one.
- **Functions of the unit's namespace, one per operation, which a backend makes the structure's
  members.** MIR then states a table from operation to function, and the C++ backend has to work out
  each member's name, whether it is static, its parameters, and a body forwarding to the function --
  and to invent `!=` and `$isunknown`'s bit itself. That is a render deciding what a declaration
  contains, which the backend contract gives to MIR.

## Consequences

- A unit states up to fifteen methods for each structure it declares, used or not. A unit naming a
  structure another unit declares states none.
- A method asked of the type takes the prototype the library's entry takes of any value, which a
  structure's own reading does not need; the runtime's table passes it too.
- A tuple a lowering composes for itself -- a completion's answer, an associative entry -- is its
  components and nothing more, and has no operations. Nothing asks a whole-value operation of one.
- The methods are built while the declaration is made, so what a method answers of the structure
  itself -- whether it can hold an unknown, how wide its stream is -- is read off its members, and a
  method asking another of the structure's own questions names that method by the declaration being
  made. A member that is itself a structure is declared first, so its methods are there to call.
- The set a `$countbits` counts under is always four positions, the first admitted value repeated,
  so a structure's count takes one type.
- A union and a tagged union are identified by their declarations too (LRM 6.22.1), but their
  operations are still composed below MIR from their members', so nothing yet reads a union's
  declaration and it is not carried.
- [a-type-owned-computation-has-no-object](a-type-owned-computation-has-no-object.md) rejected a MIR
  target keyed by a type and a reading, because enumerating the readings would let the source
  language shape MIR's vocabulary. The methods here answer operations MIR already names -- the
  equalities, the bit-stream and unknown questions -- the way an operator overload adds a type's
  answer to an existing operator, and that entry's per-unit synthesis is kept.
- `mir.md` forbids side tables that recover topology from keys and relationships held away from
  their owner. The declaration owns its methods, and a unit naming the structure reaches them
  through the declaration its type names.

## Cross-references

- LRM 4.3, 6.6, 6.7.1, 6.22, 6.24.3, 7.2, 9.4.2, 11.2.2, 11.4.5, 11.4.14, 20.6.2, 20.9
- [a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) -- where the members lie and
  how the table is reached.
- [hir-type-interning](hir-type-interning.md) -- a structure is keyed by its declaration.
- [a-type-owned-computation-has-no-object](a-type-owned-computation-has-no-object.md) -- the
  readings a type decides, which the unit's namespace owns.
- `../architecture/backend_contract.md` -- a backend chooses spellings, never operations.
