# An operation over many types is built by the lowering, and MIR holds no generic function

Date: 2026-10-24 Status: accepted; revised 2026-10-27, when the generic functions this record first
decided were taken out of MIR (see Revised)

## Context

Some operations a program needs are one operation over many types. Making a closure in a keeper's
storage and committing it is the same steps whatever the closure, and only its size, its alignment
and the values it is made of differ. Whatever depends on a program type has to be compiled with the
program: the prebuilt runtime is told sizes, alignments and the addresses of functions the design
contains, never a type to interpret ([a-packed-value-is-its-words](a-packed-value-is-its-words.md)
D7). So such an operation is either built for each place or type that needs it, or written once over
a type parameter and made concrete per type.

SystemVerilog's own parameterization never reaches MIR: the front end elaborates each
specialization, and HIR holds concrete types. So a generic definition in MIR would serve only the
compiler's own operations over a family of types, none of which a user writes or reads.

## Decision

**D1. MIR holds no generic definition, type parameter or bound, and every body it holds is
concrete.** An operation the compiler needs over many types is built where it is needed, by a
builder of the lowering into MIR, from MIR's ordinary constructs: once per site where it is made of
the site's own values, once per type where it reads the type's structure. Every rewrite, the
verifier and both backends then read concrete bodies only, and the C++ backend renders no template
of its own.

**D2. A deferred effect is made by one such builder.** At each site the keeper is found once and
held in a local, the effect is made by a placement new-expression in storage the keeper's allocation
function hands out, and the keeper commits what it made there:
`k = <keeper>; k->Commit(::new (k) Effect(values...))`. The steps and their order are stated once,
in the builder; what repeats at each site is the builder's output, as a C++ compiler repeats the
code of an implicitly defined member wherever it is used.

**D3. An operation that reads a type's structure is built per type by one rule** -- a structure's
equality, copying or ending a value, or one that computes a type from its parameters, such as an
array's stream, as wide as its count times its element's width -- as clang defines a class's
implicit special members per class and rustc builds drop glue and clone shims per type.

**D4. A generic function comes back to MIR only for an operation per-site or per-type building
cannot state as plainly**: one whose definition, checked against its bounds where it is defined,
catches what no check of a concrete body can, or whose concrete forms are many enough that one
definition costs less than all of them. It then comes back whole, with the model the first version
of this record named and did not build: a bound is that the type implements a trait, a call through
a bound is a method of it, the body is checked where it is defined to use only what its bounds
grant, it holds values of a parameter type as any body holds values, and making it concrete is the
first conversion of a unit, before any representation is decided.

## What the field does

- **clang** builds what a class does not declare itself -- its copy constructor, its destructor --
  per class, from the class's members, when a use needs it (`Sema::DefineImplicitCopyConstructor`,
  `Sema::DefineImplicitDestructor`, `clang/lib/Sema/SemaDeclCXX.cpp`). A placement `new` is a
  `CXXNewExpr` whose allocation function is found by the placement arguments' types; the library's
  `emplace` is a template only because a library has no other way to say it.
- **rustc** builds drop glue and clone shims per type, as MIR bodies of their own
  (`build_drop_shim`, `build_clone_shim`, `compiler/rustc_mir_transform/src/shim.rs`), and keeps
  generic functions for what a user wrote generic, which its monomorphization collector makes
  concrete per set of type arguments (`rustc_monomorphize/src/collector.rs`).
- Neither compiler writes a generic function of its own into its IR to express a step it could build
  per site: a generic in their IR is always the user's.

Where Lyra differs: nothing a user writes is generic by the time it reaches MIR, since the front end
has specialized every SystemVerilog parameterization, so MIR has no user generic to keep either.

## Rejected

- **Generic definitions in MIR, made concrete by the first conversion of a unit** -- this record's
  first decision, built and then removed (see Revised).
- **C++ templates in MIR**, checked once substituted. A definition then means nothing until it is
  used, an error surfaces in whichever instance first exercises it, and type-level computation turns
  the IR into a program that writes programs.
- **A node of its own for each such operation**, naming its operands and its type. It writes a
  library operation into the IR, which neither clang's AST nor rustc's THIR has where a library
  function over the existing constructs says it.
- **The operation in the prebuilt runtime, told the type's facts.** That is a description of a type
  interpreted by code compiled before the type existed, which D7 above forbids.
- **Instantiation in each backend's own way** -- the host compiler for one, the lowering to the
  execution IR for the other. The concrete forms are then not one verifiable step's output, and the
  host compiler would decide the frames of the C++ backend's instances.

## Consequences

- A unit passes through two phases, as built and legalized
  ([a-rewrite-of-mir-says-whether-a-program-needs-it](a-rewrite-of-mir-says-whether-a-program-needs-it.md)).
- The emitted C++ makes each deferred effect where it is written, as one line a reader follows
  without a definition elsewhere, and names no instance.
- What a keeper asks of the effect it keeps -- called once, after the frame that made it has gone on
  -- is the commit entry's own declaration, and the verifier holds the effect made in place to it at
  each site, as it holds every closure handed to the runtime.

## Revised 2026-10-27

The first version of this record decided that a unit defines generic functions, checked where they
are defined, with bounds a call is held to, and made concrete by the first conversion of the unit.
Built, it was one definition -- making a deferred effect in a keeper -- with two kinds of bound,
three constructs reached through them, a phase of its own, instances named from their definition and
the spelling of their types, and the verifier rules for all of it. The re-derivation asked what it
served. No user writes or reads a MIR generic, and the one definition's concrete forms were each
three nodes a builder states once; its instances read worse in the emitted C++ than the steps they
stood for, behind a name made to be unique rather than read. The earlier rejection of building the
steps at each site said they would then be stated once per site; that was wrong, since the builder
states them once and only its output repeats, which is how clang's implicit members and rustc's
shims are made. What the first version decided about the model, should a generic function earn its
place, is kept in D4.

## Cross-references

- [construct-in-final-home](construct-in-final-home.md) -- making a deferred effect in its keeper's
  storage, D2's operation.
- [a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)
  -- per-type bodies a unit states for each array type, which D3 covers.
- [a-value-too-large-for-a-frame-is-held-through-a-box](a-value-too-large-for-a-frame-is-held-through-a-box.md)
  -- the representation decision a returning generic function would be made concrete ahead of.
