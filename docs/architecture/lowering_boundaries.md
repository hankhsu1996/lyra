# Lowering Boundaries

Lyra does not yet carry out the `shuffle` array method (LRM 7.12.2), which invariant 10 places in
the library beside `reverse`; the invariant binds regardless.

## Purpose

Define what each lowering step may and may not do. Lowering is one-way, and each step has a narrow
set of allowed transformations.

A lowering exists so that a class of knowledge stops being needed downstream. AST-to-HIR retires
frontend identity; HIR-to-MIR retires SystemVerilog; MIR-to-LIR retires structured code; LIR-to-LLVM
retires the compiler's own vocabulary. Each step retires its class by **writing it down in the next
layer's vocabulary** -- everything the input layer knew that still matters is restated in the
target's own terms. Knowledge a step neither restates nor retires is knowledge every downstream
consumer must guess at, and each consumer guesses from whatever side data is nearest to hand. Those
guesses are tables, and a table has holes: the construct nobody thought of reaches a consumer that
cannot name it.

Restating is therefore local and total. Local: an input node's output is determined by that node and
its own subtree, never by a property of the tree around it. Total: every input node produces output,
including one that declares nothing. A step that must first walk its whole input to compute a
property, and only then knows what to emit, is not restating -- it is deciding, and the decision
belongs to whichever layer already held what the property was computed from.

## Owns

- The rule that lowering is one-way: a later stage does not write into an earlier stage.
- The rule that each lowering has a fixed input layer, a fixed output layer, and a fixed set of
  transformations it is permitted to perform.
- The dump and checkpoint contract: each lowering's output is inspectable in the output layer's
  canonical textual form (HIR dump, MIR dump, etc.). Dumps are debug surfaces, not compilation
  paths.
- What HIR-to-MIR makes of the SystemVerilog constructs no generic language has as a node of its
  own: a write naming several places, an array method that asks something of each entry, and an
  operand that looks as if it needed a number before the program runs.

## Does Not Own

- The internal shape of any single IR layer.
- Implementation strategy within a single lowering, and how a lowering pass is organized
  ([`lowering_organization.md`](lowering_organization.md)).

## Core Invariants

1. AST-to-HIR is the cutover from frontend identity to compiler-owned identity. Frontend ids are
   discarded at this boundary and never reach HIR or beyond.
2. HIR-to-MIR converts HIR constructs into MIR objects, members, and callables. It does not
   introduce CFG, storage placement, or scheduling.
3. MIR-to-LIR introduces CFG, storage placement, and the scheduling protocol. It does not
   reintroduce language-level constructs or re-decide ownership.
4. LIR-to-LLVM preserves the execution semantics LIR decided, selecting the concrete representation
   and runtime realization the backend requires. Design decisions are not made at this boundary.
5. Information decided at an earlier layer is carried forward, not rebuilt. Downstream layers do not
   re-derive upstream decisions from side data.
6. Lowering does not introduce new semantic identity. The target layer's identity kinds are defined
   by the target layer's contract; a lowering may only use identity kinds that belong to the input
   or output layer.
7. The pipeline branches at MIR into two backends: the LIR-then-LLVM path (the primary one) and the
   C++ emitter (a secondary realization that consumes the same MIR). A backend emit makes no
   semantic decisions -- every semantic fact is fixed in MIR -- and chooses only how to represent
   each MIR node in its target, a representation fixed by the node's kind. The two backends
   represent the same node differently (a coroutine method versus an LLVM coroutine frame) yet
   realize the same behaviour, which holds only because neither adds, infers, or re-derives a
   semantic fact. **Both backends are mechanical at the same discipline.** The C++ backend being
   secondary does not loosen this -- if its render has to choose between arms a reader could tell
   apart by running the program, the MIR is wrong, and the LIR/LLVM path would face the same
   obstruction. Two spellings of one operation are not that, and are what a backend is for. The C++
   backend's render is therefore the cross-check on MIR shape
   ([`backend_contract.md`](backend_contract.md)).
8. A lowering translates node by node, with no preliminary pass over its input. Each input node's
   output follows from that node and its own subtree; every input node produces output, whether or
   not it declares anything. A step that needs to know a property of the whole input before it can
   emit any of it has taken on a decision that belongs upstream.

   Staging a lowering into declarations and then bodies is not such a pass, and the discriminator is
   what the earlier stage leaves behind. A declaration stage **emits** -- its output is declarations
   the body stage reads by identity, each derived from its own entity, so it is a map over entities
   rather than a fold over the input. A preliminary pass instead **decides**: it computes one
   property of the whole input that the real pass then branches on, which is the decision belonging
   upstream. The staging exists because a body may reference a declaration while a declaration never
   references a body, so no ordering of a single pass can serve mutual reference
   ([`lowering_organization.md`](lowering_organization.md) states it). It is not an optimization and
   it does not weaken this invariant.

9. **What a write names is one place, or a join of them, and only HIR-to-MIR knows the difference.**
   In that lowering a write's target is an lvalue: one place, a join of lvalues, a select of a join,
   or a stream target (IEEE 1800-2023 A.8.5). A join is recursive, as the grammar is, and its kind
   says how a value is cut among its members and how what they hold is joined: packed -- a
   concatenation, or an assignment pattern of an integral type, the members sharing one vector's
   bits, the first most significant (LRM 11.4.12) -- or unpacked -- an assignment pattern of a
   structure or a fixed-size array, member `i` taking member or element `i`. A stream target cuts
   only: it holds no value to read back, and its members are what LRM 11.4.14.3 and 11.4.14.4 let a
   stream unpack into. A select of a join, which arises only where a modport's name stands for a
   concatenation (LRM 25.5.4), gives each member the whole value at the same bits counted from where
   the member begins. Every kind of write -- blocking, nonblocking, continuous, an output actual, a
   takeover -- asks the lvalue for its shares, each a place and the value it takes, in order, and
   makes its own kind of write to each; none asks which form the source wrote, and the value is held
   once before it is cut. Every place is located before the first is written (LRM 10.4.2 fixes it
   for a nonblocking assignment, and locating first makes the blocking form name the same places),
   and a write that reads first -- an assignment operator, an increment, an `inout` actual -- reads
   what the members hold together, each member's index evaluated once (LRM 11.4.1). MIR receives a
   local holding the value, reads of its parts and writes to one place each; a write to one place
   stays the store it was. _Requirement: whatever a write names, each destination receives its share
   of one value by that same kind of write, with the value evaluated once and every destination
   located once, and two backends cannot cut it two ways because neither cuts it._
10. **An array method that asks something of each entry is a loop at its call.** The locator,
    reduction and `map` families (LRM 7.12.1, 7.12.3, 7.12.5) are a loop over the receiver's entries
    in their natural order, with the `with` clause evaluated at each as ordinary code of the body
    the call is in, the element and the index variables of that loop; the index is the entry's own
    -- the key, the position, or the index a fixed-size array's declaration gives it (LRM 7.12.4). A
    sort gathers each entry's key the same way, orders the keys by a body stated once for the key
    type with that type's own relational operator, ascending or descending, as a stable merge over
    positions, which reads only within the runs it merges even where a 4-state comparison answers
    neither way, and the array's type then applies that order by a body of its own (LRM 7.12.2).
    What a method answers is the call's type, starting at that type's default -- a reduction's first
    entry replaces it, so an empty receiver answers the default (LRM 7.12.3 states none). `reverse`
    and `shuffle` ask nothing of an entry and stay the library's permutations. _Requirement: the
    clause is an expression of the program at a type the program chose, which only the compiler, at
    the call, can compile; Rust's generic `Iterator::sum` and Verilator's templated `r_sum` are the
    same code instantiated at the use, and a library compiled before the program instantiates
    nothing._
11. **An operand is lowered as the expression it is, and a number a lowering needs before the
    program runs is read from a type.** A replication's multiplier and an assignment pattern's count
    are operands handed to the repeat whatever their value; the language classifies them by whether
    they describe a value's shape or its contents, and puts every shape in a type, so a width, a
    dimension's element count and a structure's member count are read from the type. A pattern's key
    is a designator naming a position. Where the language itself asks whether something is fixed
    before the program runs -- whether LRM 20.7's dimension index makes a query an elaboration-time
    constant -- the front end's answer is read and not recomputed. An integral operation whose
    operands are all constants of the unit is folded where it is built, by the builder every
    operator of the lowering is made through, which asks the value library the run itself uses, so a
    folded and an unfolded operation cannot disagree; an operation that can fail while running is
    not folded. _Requirement: the lowering accepts every text the front end accepts --
    `{Dw-1{1'b0}}`, LRM 11.4.12.1's own idiom, reaches it as arithmetic over a parameter -- and
    holds one evaluator, the one the run uses, beside the front end's._

## Boundary to Adjacent Layers

- Each lowering produces output that is valid in the target layer's contract. The output passes the
  target layer's invariants immediately; there is no intermediate "almost-HIR" or "partial MIR"
  state exposed to other stages.

## Forbidden Shapes

Each entry names what it fails. The test for a new one is the same: which of the properties in
Purpose -- one-way, local, total -- or which invariant above does it break.

- **A lowering pass that writes data back into its input layer** fails one-way lowering.
- **A lowering pass that skips an IR layer** (HIR to LIR directly) fails the rule that each layer
  retires one class of knowledge: what the skipped layer restates is then restated by nobody.
- **A lowering pass that produces output violating the target layer's invariants and defers fixup to
  a later pass** fails the boundary rule above: every consumer between the two sees a state no
  contract describes.
- **A lowering pass that reconstructs upstream ownership from string names or coordinates** fails
  invariant 5.
- **A lowering pass that depends on runtime identity or per-instance data at compile time** fails
  the compile-time / runtime split ([`compiler_overview.md`](compiler_overview.md) invariant 4).
- **A lowering pass that invents a new identity kind not defined by either the input or output
  layer's contract** fails invariant 6.
- **A lowering that computes a property over its entire input before it can translate any part of
  it** fails invariant 8: the property is a decision, and it belongs to the layer that already held
  what it is computed from.
- **A predicate inside a lowering that gives one source construct two different output shapes**
  fails totality: every downstream consumer then carries both shapes, and the one a consumer forgets
  is a defect that the other shape hides.
- **An entity whose output placement is decided by a property of what its descendants contain rather
  than by what it declares itself** fails locality: placement that flows outward obliges a later
  stage to record where each thing went, and every consumer to consult that record.
- **An output node emitted only when its input node is non-empty** fails totality: zero is a count,
  not a different shape, so a construct that declares nothing produces the same output as one that
  declares many, with an empty list.
- **An output node that omits a fact the input node stated, leaving a consumer to recover it from
  the node's type, its name, or its position**, fails the purpose of restating: the consumer's
  recovery is a table over the cases someone enumerated, and the case nobody enumerated is a hole.
- **A join of places as a write target in MIR**, distributed by each backend, fails invariant 9: the
  cut is then made in two places that must agree, and nothing holds them alike. Clang's AST does
  hold such a target, a vector swizzle, and its code generation expands it -- which is the layer
  that answers here.
- **A write that reads first by naming its target twice** -- `l = l op r` with the left side cloned
  -- fails LRM 11.4.1, which evaluates the target once: an expression named twice is evaluated
  twice.
- **A join kept as a special case of one kind of write**, the assignment statement, fails invariant
  9: every other position the language lets a join stand in becomes a refusal.
- **A library entry handed a body of the program to run once per entry, answering a value of a type
  the program chose**, fails invariant 10: the library was compiled before the program and cannot
  name the type, so every key crosses erased and every fold, comparison and test is asked of a type
  record at run time.
- **A value-producing array method that derives its answer's type from the receiver** fails
  invariant 10: an index locator on an associative array answers a queue of the key type, a `map`
  answers the type its clause chose, and an empty reduction must still answer a value of the call's
  type.
- **Matching an operand for an integer literal where a number is wanted** fails invariant 11: it
  refuses arithmetic over a parameter, which the front end leaves as arithmetic.
- **Evaluating an operand where a lowering wants a number, or carrying on a node a number its type
  states**, fails invariant 11: the first is a second evaluator that has to choose a scope to
  evaluate in, and the second states a shape the node's own type states, the two told apart only by
  trusting them to agree.

## Notes / Examples

If HIR-to-MIR needs to emit a CFG node, the boundary has been violated. Either the node belongs in
MIR in a non-CFG form, or the lowering is doing MIR-to-LIR's work.

When extending HIR-to-MIR, the lowering's output is also validated against
[`backend_contract.md`](backend_contract.md)'s mechanical-translation invariant: the produced MIR
must be translatable by a mechanical LLVM IR backend without that backend having to work out which
operation the node names. The C++ backend's role as the observation surface for MIR makes this check
operational -- if consuming a proposed MIR shape would leave a render choosing between arms that
behave differently, the MIR is wrong, and the lowering is what produced it.

A join, as the lowering states it, is Rust's destructuring assignment (`rustc_ast_lowering` rewrites
`(a, b) = t` to `{ let (l1, l2) = t; a = l1; b = l2; }`) widened by the one form Rust, Python and Go
refuse: an assignment operator on it, which LRM 11.4.12 makes meaningful by calling a concatenation
a packed vector of bits. In `{i, a[i]} = v` the second member is the element `i` named when the
statement was reached. A stream target's members are filled from the most significant end, and a
`with` range on one is evaluated just before that member is unpacked, so it may read a member
unpacked ahead of it (LRM 11.4.14.4); a dynamically sized member takes what the members after it
leave. A select of a join is a node of the lowering rather than a view in the library, because a
view generic over how many places a use joins is generic over the use, which only the compiler can
spell out.

The axis invariant 11 turns on, in one table:

| The operand                           | What it describes                    | What the lowering does          |
| ------------------------------------- | ------------------------------------ | ------------------------------- |
| `{4{w}}` multiplier, packed result    | shape: the result's width            | hands it to the repeat          |
| `{n{"ab"}}` multiplier, string result | contents: a string has no width      | hands it to the repeat          |
| `'{N{items}}` count, array target     | how many elements the value has      | hands it to the repeat          |
| `'{N{items}}` count, structure target | shape the member list already states | nothing: reads the member count |
| `'{2: v}` key                         | a designator, like a member name     | carries the position it names   |
| every value written in a pattern      | contents                             | lowers the expression it is     |

The two replications are not a constant and a run-time multiplier: the result type fixes the size in
one and not the other, which is why LRM 11.4.12.2 allows a run-time multiplier over a string and
11.4.12.1 does not over packed bits.
