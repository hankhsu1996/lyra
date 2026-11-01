# A type the library declares is named by its declaration

Date: 2026-10-31 Status: accepted.

## Context

MIR held, beside the types its own constructs make, take apart or lay out, a dozen types of the
runtime library it only names: a string, a value of any type, the dynamic array, the queue and the
associative array, a run of bits, a print item, a wait, a cancellation target, a scope, the
structures a unit states constants of, and others, each its own alternative or an enumerator of one
"runtime library kind". What every layer had to know of one -- whether a temporary may stand in for
it, how large a frame holding it is, whether it is storage reached only where it lies, what a member
holding it is, how each backend spells it -- was answered by an exhaustive switch over the kinds in
each place that asked: five in MIR, three in LIR, the translation between the two, both backends'
type mappings, the LLVM backend's member storage and record layout, and the DPI header. Adding a
library type meant editing all of them, and nothing tied the answers to one another.

## Decision

**D1. A type is MIR's own when MIR's constructs make, take apart or lay out its values, or a rule of
the language reads its parts. Every other type is a declaration of the library, named with the type
arguments the language gives it**, as rustc's `Adt(def, args)` names a definition of a library
crate. MIR has one alternative for it, the declaration and its arguments, and LIR mirrors it; both
are one shape of the library every layer depends on, instantiated at each layer's own type, so an
argument is read the same way in both. An argument is a type, or a value where the declaration takes
one (a queue's bound, LRM 7.10).

**D2. Whatever any layer asks of such a type is stated once, by its declaration, and read there.**
The declarations are a table beside the layers that read them -- MIR, LIR, both backends -- in the
library every one of them already depends on. Each states:

- its semantic part: its parameters and what a rule of the language reads each as -- a type, a
  container's element type, an associative array's index type, a queue's bound -- whether a variable
  may be declared to hold one (LRM 6.2), its movability, the entry building the value one starts as,
  and the constructor a construction of one reaches;
- its ABI part: how a holder holds it -- as an object of the value domain a value of the design is
  realized in, in storage whose size and alignment the declaration states; as an object of its own,
  whose entries carry the name the declaration states, held by value in storage it states or only as
  a member of an object holding it whole; or as the address of storage kept elsewhere -- what a
  member holding one is (none, the value it is, its own object, a handle on what outlives the
  member, or a history over the values it holds, which names the argument that is their type), the
  C++ type a holder names it by (a class, a class template its arguments are written after, or a
  reference to a facility) and the C spelling a DPI-C prototype names, whether it is a class of the
  runtime's and how long an object of that class lives -- named by the one enumeration of lifetimes
  MIR also reads off a class of the source -- and the entries it keeps deferred effects by where it
  is a keeper.

A declaration cannot state a pair a holder could not realize: the value it is is a member only of a
value of a domain, its own object only of a declaration stating one, which is then held by value or
as a member, and a handle or a history only over storage kept elsewhere; a declaration stating
another pair fails to compile. An object of its own always names its entries, so nothing asking for
them has a case with none.

A declaration held by value states its storage itself, and the runtime asserts it against every
class a backend builds there: the class the library's own entries build, which the execution backend
calls, and the class the C++ backend writes, which is another where it writes a class template over
the arguments -- a container -- or a class of its own for the same value -- a wildcard index. The
storage stated is exactly the least every one of them fits; every backend gives a holder that
storage, so a frame holds what MIR counted; and the check is made where the library is compiled and
where a program built against it is, so a size is stated once. The runtime pairs each declaration
with those classes in one listing, which every check reads, and so does the layout of a member
holding a declaration's own object; a declaration whose object is held by value or as a member, and
is missing from that listing or listed twice, fails to compile, as does one listed whose object is
neither. An object the library defines for a type MIR owns -- an execution, which lies in its
maker's storage until whoever drives it takes it, a reference, a counted hold, a write in progress
and a part designated within one, a write into an object's properties, a class handle -- states its
storage beside the enumeration of such objects, keyed by the object, and is checked against the
classes each backend builds the same way, so every holder gives every such object the storage stated
for it. What a layer derives from these -- whether a value of one lies in storage its holder keeps a
claim on, whether it is storage reached only where it lies, the runtime object a holder builds and
ends, the name its entries carry, which domains the execution backend realizes an entry built for a
declaration for -- is a function of them, never a second statement. No second enumeration of the
runtime's objects spells the declarations again: an object a declaration states as its own, and a
class of the runtime's, is named by that declaration wherever the runtime is asked about it -- a
member holding one, the part a class of the source extends, and the identifier that part crosses the
runtime's interface by.

An argument is read by the role its parameter plays, never by its position: a reader that needs an
argument asks for it and is refused where the declaration takes none, and a reader for whom it is
optional asks whether there is one and is answered that there is none. MIR's verifier refuses a type
named with arguments its declaration does not take.

**D3. A construction of a type the library declares reaches the constructor its declaration names**,
an entry of the shared runtime-entry declaration that no call reaches, as a C++ class's constructor
is reached by naming the class. A target whose construction names the type writes it; the execution
backend calls the entry, named as the shared declaration names every other entry -- once, per kind
of value it builds, or per kind of value it is built over. **What a declared variable of one starts
as is built by the entry the declaration names for it**, which takes no operands and is called at
the type it builds, as a C++ class's value-initialized object is made; a type the declaration names
none for holds nothing a declaration could ask for. MIR calls that entry, and no lowering chooses
operands for a starting value by the kind of type.

**D4. A structure of the library a unit states constants of is listed once, beside the structures
themselves, with the declaration naming it.** MIR reads each member's type off that structure, and
the execution backend reads each member's place off it, so the two read one declaration of the
record and neither restates its members.

## Survey

- rustc (`compiler/rustc_type_ir/src/ty_kind.rs`): `TyKind` holds the language's structural kinds,
  and every library type is one kind, `Adt(AdtDef, GenericArgs)`; what the compiler must know of a
  particular one is a flag set once on its `AdtDef` (`IS_BOX`, `IS_PHANTOM_DATA`,
  `compiler/rustc_middle/src/ty/adt.rs`), never a match over library types at each use.
- clang: `std::string`, `std::vector`, `std::any` are records of headers, not semantic types; a
  library type a language rule names (`initializer_list`, `type_info`, `source_location`) is looked
  up by name in Sema, cached, and its shape checked once.
- Go (`cmd/compile/internal/reflectdata/map.go`) keeps a copy of the runtime's map layout in the
  compiler, kept in step by a comment. Lyra cannot read the library's source, so its sizes are
  stated -- once, on the declaration -- and the runtime asserts each against its class, which is
  what Go's comment does not do.

## Rejected

- **An enumeration of the classes of the runtime's beside the declarations**, each declaration
  naming its enumerator: the pairing of each with its class, the lifetime MIR reads of one and the
  identifier the part a class of the source extends crosses by are then each a switch over that
  enumeration, a second statement of the same declarations.
- **A kind enumeration with a switch per question**, the form this replaces: every question answered
  by its own switch, so a new library type touched every one and two of them could disagree.
- **A pointer to a declaration as the identity**, as an `AdtDef` is interned. An enumerated
  declaration is as cheap to compare and also names itself in a symbol and a dump the same way on
  every run.
- **A switch over the declarations at the execution backend picking each one's construction entry.**
  It answers from the backend a question each type's declaration answers, and nothing holds the two
  in step.
- **A kind per container in MIR beside the declarations**, kept because the language gives
  containers rules of their own -- construction from an element list, keyed iteration, leasing an
  element, a queue's bound. Every one of those rules reads an argument (the element type, the index
  type, the bound) or whether there is one, so a parameter's role answers it, and no rule reads the
  container's representation.
- **A starting value built by the constructor out of operands chosen per kind** -- an empty element
  list for a container, empty text for a string. Which operands mean "nothing" is a fact of each
  type, so the lowering choosing them restated, per kind, what the declaration is for.

## Consequences

- Adding a library type is adding its declaration, the entries it names, its own operations, and,
  where the runtime defines a class for it that a holder or a member gives storage of its own, the
  one line pairing the declaration with the classes each backend builds there, without which the
  runtime does not compile; a class of the runtime's is likewise listed once with its class; a data
  type is also added to the test stating which declarations a variable may hold (LRM 6.2). No switch
  over the types the library declares is edited anywhere else.
- A rule of the language that names one container -- `new[N]` on a dynamic array, a queue's
  `push_back` -- asks whether a type is declared by that declaration; a rule over every container
  asks the declaration's parameters.
- What a member of an object holding one is, is what its declaration states for it: a value of a
  domain held as the value it is, the declaration's own object, a handle, or a history over the
  domain of the values it holds.
