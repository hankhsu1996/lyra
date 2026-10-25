# A unit defines generic functions, checked where they are defined and made concrete before the backends

Date: 2026-10-24 Status: accepted; implemented in part (see the last section)

## Context

Some operations a program needs are one operation over many types. Making a closure in a keeper's
storage and committing it is the same three steps whatever the closure, and only its size, its
alignment and the body that builds it differ. Whatever depends on a program type has to be compiled
with the program: the prebuilt runtime is told sizes, alignments and the addresses of functions the
design contains, never a type to interpret
([a-packed-value-is-its-words](a-packed-value-is-its-words.md) D7). So such an operation is either
written out again at every site that needs it, or written once over a type parameter and made
concrete per type.

SystemVerilog's own parameterization never reaches MIR: the front end elaborates each
specialization, and HIR holds concrete types. So what a generic definition in MIR serves is the
compiler's own operations over a family of types, which until now lived in the C++ that builds them,
in the value library's templates, and in the execution backend's code generator.

C++ and Rust both write such an operation once, and differ in when its body is checked. A C++
template is checked once substituted; C++20 concepts constrain only the call, so a body may use what
its constraint did not grant and fails at instantiation. A Rust generic is checked where it is
defined: its `where` clause bounds what the body may do with each parameter, and the call is held to
the same bounds. rustc's monomorphization collector then makes "from zero to N artifacts" of each
generic function, one per set of type arguments a concrete body uses
(`rustc_monomorphize/src/collector.rs`); its codegen substitutes as it reads the generic body
(`rustc_codegen_ssa/src/mir/mod.rs`, `FunctionCx::monomorphize`). No compiler surveyed opens an IR
of its own for the concrete forms: clang adds them to its AST, Swift specializes SIL to SIL, Nim's
`generateInstance` (`compiler/seminst.nim`) copies a generic proc into a concrete one.

## Decision

**D1. A callable of a unit may be generic over types, and is checked where it is defined.** Each
type parameter states what the definition asks of the type a call names for it, as a Rust `where`
clause does, and every such bound says one thing: that the type implements a trait, a named set of
operations each type states its own implementation of. A call the body makes through a bound calls a
method of that trait on a value of the parameter type, and making the definition concrete resolves
it to the implementation of the type the parameter is made at. So being a closure called at a stated
signature and keeping deferred effects are two traits, not two kinds of bound, and calling the
closure, a keeper's allocate and its commit are three methods, not three kinds of call. The body may
use only what its bounds grant, and a call is held to the same bounds. No bound computes a type:
what would need one is not a generic's work (D4).

**D2. A generic body holds values of a parameter type as any body holds values** -- as parameters,
locals and what it answers. How such a value is stored and passed is decided once its type is
concrete, by the step that decides it for every value. Moving or copying a value of a parameter type
is something a bound grants rather than a default, because a closure in Lyra is never relocated once
made. A closure a generic calls once is consumed where it lies.

**D3. Instantiation is a pass of the lowering from the dialect MIR is built in to the dialect the
backends read.** It makes each generic definition concrete at each set of type arguments a concrete
body calls it at, on demand and cached, and does nothing but substitute types and resolve each call
through a bound to the closure or entry the type answers it by. It runs before the representation of
any value is decided, so that decision is made over concrete bodies only, and both backends receive
concrete bodies: the C++ backend renders instances, not templates. Instantiation decides which
concrete program runs; it never decides how a value is stored.

**D4. An operation is a generic definition exactly when it can be written using only what its bounds
grant.** Keeping a deferred effect is; an array's equality over its element's equality is. One that
has to list a type's members -- a structure's equality, copying or ending a value -- or compute a
type from its parameters -- an array's stream, as wide as its count times its element's width -- is
built per type by one rule, as rustc builds drop glue and clone shims per type
(`rustc_mir_transform/src/shim.rs`) and Rust's `derive` expands per declaration.

## Rejected

- **C++ templates in MIR**, checked once substituted. A definition then means nothing until it is
  used, an error surfaces in whichever instance first exercises it, and type-level computation turns
  the IR into a program that writes programs.
- **A node of its own for each such operation**, naming its operands and its type. It writes a
  library operation into the IR, which neither clang's AST nor rustc's THIR has where a library
  function over the existing constructs says it.
- **The operation written out at every site that needs it.** The steps and their order are then
  stated once per site.
- **The operation in the prebuilt runtime, told the type's facts.** That is a description of a type
  interpreted by code compiled before the type existed, which D7 above forbids.
- **A generic body restricted to reaching a parameter-typed value through a pointer.** It writes a
  limit of where the representation is decided into what the language can say.
- **Instantiation in each backend's own way** -- the host compiler for one, the lowering to the
  execution IR for the other. The concrete forms are then not one verifiable step's output, and the
  host compiler would decide the frames of the C++ backend's instances.

## Consequences

- One instance of a definition per set of types per unit; two units naming the same types each make
  their own, as each translation unit instantiates a template.

## What is implemented today

The generic callable, its two bounds, a call naming its type arguments, a call through a bound, and
substitution are built, and keeping a deferred effect is the one generic definition. Five parts of
the decision are not yet carried out:

- There are no traits: each bound is a construct of its own (one for a called closure, one for a
  keeper), and each operation reached through one is a construct of its own (a call of the closure,
  a keeper's commit, a placement's allocation through the keeper) (D1).
- Instantiation is done by the lowering to the execution IR, and the C++ backend renders the
  definition as a template, so it runs after the rewrites that keep large values off the stack
  rather than before (D3).
- Because of that, a generic body reaches a value of a parameter type only through a pointer (D2).
- A body is held to its bounds where it is defined only in what it reaches through them -- each
  operation names a parameter of its own whose bound grants it -- and nothing yet checks that it
  does nothing else with a value of a parameter type (D1).
- The one generic definition borrows the closure it calls once rather than consuming it (D2).

## Cross-references

- [construct-in-final-home](construct-in-final-home.md) -- the first generic function: making a
  deferred effect in its keeper's storage.
- [a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)
  -- per-type bodies a unit states for each array type, which D4 sorts.
- [a-value-too-large-for-a-frame-is-held-through-a-box](a-value-too-large-for-a-frame-is-held-through-a-box.md)
  -- the representation decision D3 runs instantiation ahead of.
