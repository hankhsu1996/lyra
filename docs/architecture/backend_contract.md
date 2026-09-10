# Backend Contract

## Purpose

Define the mechanical-translation contract a backend must satisfy when consuming per-unit MIR.

The architectural target of the compiler is HIR -> MIR -> LIR -> LLVM IR. The C++ backend is a
transitional realization: it consumes the same MIR but renders to C++ source instead of descending
through LIR and LLVM IR. The C++ backend's emitted output is **how MIR is observed** -- a developer
reads the emitted C++ to validate that MIR's shape faithfully represents the source SystemVerilog
through MIR's semantic model (`compiler_overview.md`).

The transitional status does **not** loosen the mechanical-translation discipline. The contract in
this doc is exactly the discipline an eventual LLVM IR backend will need: every render rule is a
fixed function of one MIR node. If the C++ backend's render has to work out which operation a node
names -- inferring a construction shape from a payload, matching on what an operand happens to be,
reading past the node for something the node did not say -- the LLVM IR backend has to work it out
too, and the two answers are held in step by nothing. Both backends pay the cost; the cost is a MIR
design failure visible from the backend side.

The C++ backend's render therefore serves as the cross-check on MIR shape today: any place where its
render is not a mechanical single-node translation is a place where the next stage (LIR / LLVM IR)
will hit the same obstruction. The bug is in MIR, not in render.

**The finished shape, which the work toward this contract is measured against.** A value-emission
entry is punctuation around the renders of its own children and nothing else: a piece of the
target's syntax with those renders filled into it. Every name it would otherwise have to know comes
from somewhere that owns naming -- a type's spelling from type mapping, a runtime operation's from
the runtime-entry declaration the layers above share, a wrapper's access protocol from the
place-access dispatch. So the end state is observable rather than a matter of judgement: **no
value-emission entry names a runtime library identifier at all**, and a reader checks that by
reading the entries rather than by reasoning about what they decide. A string literal in one that
carries anything but target syntax is the defect, whatever it was written for.

A backend that has reached it holds no table of its own. Gaining a runtime operation, a value
family, or a container kind changes the shared declaration and reaches every backend at once;
nothing in an emitter has to be found and edited to match. That is the same property from the other
side as the rule that a backend never decides what a node means -- an emitter with nothing to look
up has nothing to decide -- which is why this shape and the invariants below are one statement read
two ways.

## Owns

- The principle that a backend render entry is a **fixed function of one MIR node**: input is the
  MIR node and its structural context (its children, the node consuming it); output is determined by
  that input alone. The recursion structure follows the MIR node tree; the render of each node
  composes target-language wrapping around recursive renders of its child nodes.
- The boundary between three backend entry kinds:
  - **Type mapping** -- one dispatch per MIR type variant returning a target representation (a
    target-language type literal for C++, a size + an LLVM type for LLVM IR). This is the only entry
    that names a target-language runtime library type.
  - **Place access** -- one dispatch per MIR type variant returning how the storage behind a place
    of that type is named as an lvalue, which is what a by-reference binding and the owner of a
    descending write both compose onto. This is the only entry that names a runtime library's access
    protocol. Reading what a wrapper holds and replacing the whole of it are calls rather than
    accesses, so neither is one of its answers.
  - **Value emission** -- the entries that translate MIR expression, statement, member, and body
    nodes into target-language form. They compose mechanical syntactic wrappers around recursive
    renders; they make no decisions about what the program means.
- The contract that MIR's primitive set is closed for backends: if a backend's render entry cannot
  produce its output from the node's structural fields plus subordinate renders, the missing
  semantic is upstream (MIR's primitive set or HIR-to-MIR's lowering), not in render.

## Does Not Own

- Which primitives MIR has (see `mir.md`).
- Which target language a backend chooses.
- The per-artifact and cross-unit boundary (see `emission_model.md`).
- The runtime library a particular backend chooses to wrap target-side storage realizations in.
- Where the runtime library lives and how a binary locates it (see `runtime_distribution.md`).

## Core Invariants

1. **A backend render entry is a fixed function of one MIR node.** Two MIR nodes of the same kind
   with the same structural fields produce the same target-language output, every time, in every
   backend.

2. **A value-emission entry chooses a spelling, never an operation.** Write down what each arm of a
   branch emits. If a reader could tell the arms apart by running the program, the branch chose an
   **operation** -- a semantic decision MIR failed to state, which HIR-to-MIR then left render to
   fabricate. The fix is upstream: state it, and render becomes uniform. If only the target-language
   text differs, the branch chose a **spelling**, which is render's own business, provided it
   dispatches on a fact the node or its type states rather than on one it works out.

   A condition with more than one input is more than one branch. A conjunction, and an entry that
   takes a discriminating parameter beside the node, each carry a decision per input; judge every
   input on its own, because settling one leaves the rest choosing at the same site and the site
   then reads as though it were done.

   Presentation is neither, and has no claim: a branch that only makes the emitted text shorter or
   avoids a construct a reader would find redundant decides nothing, and the emitted artifact is not
   read for its looks.

   Different syntactic shape is therefore not the test, and reading it as one condemns the
   mechanical cases. A member reached through a pointer and one reached inline, a declaration that
   introduces a virtual slot and one that overrides it, an ordinary return and a coroutine's
   completion -- each is two spellings of one operation, dispatched on a fact MIR states, and each
   is exactly what this entry kind is for.

   Where an occurrence stands -- in value position or in target position -- is structural context,
   and dispatching on it is a spelling exactly when both readings are one operation seen two ways: a
   product answers with the component either way, so only the receiver's own form differs. Where
   they are not one operation -- where writing the part does something reading it does not, such as
   making the part it names the live one -- position selects an operation, and that is the same
   defect as selecting one from an operand's type. Run the test on it; "value category" is not a
   reason to skip running it.

3. **Type mapping is the only entry that names a runtime library type literal.** Every MIR type
   variant maps to a target-language type representation through one dispatch. Value emission
   entries render types only via that dispatch; they never compose a target-language type literal
   directly. A runtime library type's spelling lives at one place; nowhere else.

   A target also needs types for what MIR states as **structure** rather than as a value -- an
   extent whose exit is an effect, the root a managed object is emitted over -- and no MIR type
   variant names those, so no dispatch reaches them. They belong here even so, and for the reason
   this entry exists rather than by exception: each is a library type's spelling, and this is where
   a library type is spelled. What they are not is a dispatch. Nothing computes which one a site
   wants -- the site knows -- so each is named outright, and giving them a selector to be looked up
   by would be a lookup with no question in it.

4. **Place access is one dispatch per MIR type variant, exhaustive over capability wrappers.** MIR
   states that the storage a wrapper represents is reached by dereferencing the wrapper's place
   (`mir.md` invariant 14); it does not state what that costs in a target. Each backend supplies it
   from the place's type through a single dispatch, sibling to type mapping, answering one question:
   how the storage behind a place of that type is named as an lvalue. Two things compose onto that
   answer -- a by-reference binding, and the owner of a write that descends into the value -- and
   they are the same step, which is why there is one entry rather than one per consumer. Value
   emission asks that dispatch and never inspects the wrapper kind itself. Reading a wrapper's own
   value, rebinding it, and taking its address name the bare place and reach no protocol, so they
   have no entry here.

   Replacing the whole of what a wrapper holds, and reading it, reach no entry here either, for the
   opposite reason: each acts on the wrapper rather than naming its storage, so each is a call in
   MIR and is realized by the same entries that realize every other call. Their target-language
   spelling comes from where every call's spelling comes from, which is what keeps one runtime
   method named at one site (invariant 3). A write that descends is not among them: what descends is
   a run of calls, and only the owner they start from reaches this dispatch.

5. **Member declaration is (name, type) -- nothing else reaches member render.** A member's
   target-language declaration form is determined by its name and its type alone (the type carries
   size, offset, and target type form; the name carries the source identifier). Wrapper-typed
   members are no exception: any per-member construction state arrives later as ordinary MIR
   expressions in the constructor body, never as type payload that member render reads.

6. **The LLVM IR backend is the canonical cross-check.** When invariant 2 leaves a branch in doubt,
   ask: could a mechanical LLVM IR backend translate the same MIR node without working out what the
   node means? If not, the MIR shape is wrong. The C++ backend's transitional status does not relax
   this check; it sharpens it, because the C++ backend's output is how MIR's correctness is
   currently observed.

   What the check predicts is the failure a fact MIR declines to state always produces: each
   consumer works it out alone, from whatever is nearest to hand, and the answers agree until the
   day one of them does not. Nothing holds them in step, and the consumer that answers differently
   is a wrong answer no reader is positioned to see -- which is why the search worth running is not
   "where does render branch" but "which question is answered in more than one place".

7. **The set of backends consuming MIR is open.** No MIR primitive and no contract entry is
   specialized for one backend. A new backend reads the same MIR; the only thing it brings is its
   own type-mapping and place-access dispatches and value-emission rules for its target's syntactic
   form.

8. **A value-emission entry names no runtime library identifier.** Every name it emits is either the
   target language's own syntax or the answer of a dispatch that owns naming: type mapping for a
   type, place access for a wrapper's access protocol, and the shared runtime-entry declaration for
   an operation. The entry looks nothing up itself; it composes punctuation around what its children
   render to. This is invariants 2 and 3 read forward rather than as prohibitions, and it is what
   makes the contract checkable by reading a render entry instead of reasoning about it -- a
   property the earlier form did not have, which is why a spelling written into an emitter went
   unnoticed for as long as it read like the emitter beside it.

## Boundary to Adjacent Layers

- `compiler_overview.md` defines the pipeline (HIR -> MIR -> LIR -> LLVM IR) and the transitional
  status of the C++ backend within it.
- `mir.md` defines the primitive set this contract realizes. A render entry that needs anything
  beyond the node's structural fields is one of two failures: a missing MIR primitive (extend MIR)
  or a missed HIR-to-MIR lowering (extend HIR-to-MIR). Render absorbs neither.
- `lowering_boundaries.md` requires HIR-to-MIR to produce MIR that satisfies this contract: the
  output MIR must be mechanically translatable by any backend, including a mechanical LLVM IR
  backend.
- `emission_model.md` owns the per-artifact and cross-unit boundary. Within an artifact, this doc
  owns the per-node translation rules.
- `runtime_distribution.md` owns where the runtime library lives; this doc owns how a backend may
  reference runtime library types (only through type mapping).

## Forbidden Shapes

- **A value-emission branch whose arms differ in what the program does.** This is the canonical
  render-side defect: render deciding what should be a MIR-level distinction. The fix is upstream,
  never inside render.

- **A value-emission branch that exists only to shape the emitted text** -- collapsing a construct
  the producer built, avoiding one a reader would call redundant, shortening a form. It decides
  nothing and states nothing, so what it costs is a branch that must be read and kept correct, and
  what it buys is not something the artifact is for. Where the collapsed form is the right one, the
  producer is what states it.

- A value-emission entry that composes a target-language type literal as a string. Every
  target-language type literal a backend emits comes from the type-mapping dispatch.

- A value-emission entry that names a runtime library identifier (wrapper type, helper function,
  helper struct, method spelling) directly. The runtime library's identifiers appear at render
  through the MIR types and MIR calls that map onto them.

- A value-emission entry that switches on a MIR type's payload to choose a non-mechanical emission
  form. Type payload is for type-mapping; value emission acting on it reads value-layer data carried
  in a type.

- A value-emission entry that branches on which capability wrapper a place's type is -- an
  observable arm, a driver arm, a reference arm -- whether in value render, assignment render,
  projection render, or argument render. A wrapper's access protocol comes from the place-access
  dispatch, at one site. Spread across emitters it is the same defect as a runtime library name
  escaping type mapping, and it obliges every emitter to grow an arm each time a wrapper is added.

- A member render entry that emits constructor arguments built from the member's type payload. A
  member is (name, type). Construction state arrives as ordinary MIR primitives in the constructor
  body, never as type payload.

- A render entry whose output depends on the node tree outside the node's own structural fields and
  its subordinate renders.

- An architecture doc, a glossary entry, or a MIR invariant that names a backend's runtime library
  type by its target-language spelling. Those names are implementation detail of one backend's
  choice; architecture refers to MIR types and their type-mapping role.

- A new MIR node or MIR primitive added to make one backend's render simpler. MIR's primitive set is
  uniform across backends. A primitive earns its place by being a generic programming-language
  concept (`mir.md`), not by being convenient for one backend.

- A render entry that fabricates an expression from MIR data (composing constructor arguments,
  inlining a struct literal, deriving navigation steps from a payload). Render translates existing
  expressions, never invents them. An invented expression is HIR-to-MIR's job.

## Notes / Examples

The canonical write rule for any value-emission entry: write it as if the target were LLVM IR. If
you cannot write a mechanical translation rule for it -- if your draft contains an `if` whose arms
produce different LLVM instruction sequences -- the MIR primitive set is incomplete. Fix MIR; render
then writes itself.

What invariant 8 looks like at the two sites that carry the weight. An expression entry is one arm
per node kind, each a piece of the target's syntax with the children's renders in it -- a binary
operator is its token between two renders, a dereference is the target's dereference around one, a
composite is the type-mapping answer around the renders of its parts. A call entry is two steps and
no more: what the target names the callee, and how it composes an object, a name, and a list of
rendered operands. The callee's name for a runtime operation is one lookup in the shared
declaration, which already says whether a call site reaches the operation as a free function, as a
method on the object it acts on, or as a factory on the type it builds -- so the entry chooses
nothing and knows no identifier.

An operand is the one thing a call entry may not read past. Where a call carries something the
target spells somewhere other than its argument list -- a position fixed where the call is written,
which a typed target resolves where it resolves types -- that thing belongs to the callee rather
than among the operands, because reading an operand back to decide how to spell it is the same
defect as reading one to decide what the call means. The tell is arithmetic: a fact placed among the
operands that does not belong there costs one special case at every site that walks operands, and
the count of those special cases is the measure of how wrong the placement is.

Reading what a capability wrapper holds, and replacing the whole of it, are both calls, because MIR
states them as calls: each acts on the wrapper rather than naming its storage. Render composes them
the way it composes every other call, so each method's spelling comes from the one place every
runtime entry's spelling comes from, and no access entry writes a second copy of it. Neither render
knows the wrapper's name or which wrapper kind it is.

Writing part of what it holds descends through it: one call per level, each naming the entry the
lowering settled and taking the level above it as its receiver, with an ordinary assignment at the
end. Only the owner those calls start from reaches the place-access dispatch. How a target realizes
such a call is its own answer -- reaching into the storage in place, or reading the whole value,
rebuilding it and storing that back -- and it is a property of that target's value representation
rather than a decision taken per site, so neither form is visible above render and neither is chosen
by an emitter.

Naming that storage as an lvalue is not a call: it names storage rather than operating on it, and it
is the one question the place-access dispatch answers. Two things ask it and both compose onto the
same answer -- a by-reference binding, and the owner of a write that descends.

Rebinding a wrapper renders as an ordinary assignment to the bare place, reaching no protocol and no
call, because MIR states rebinding and writing-through as different nodes.

A wrapper-typed member's construction follows the same shape. Construction state arrives as ordinary
MIR primitives in the constructor body (a `CallExpr` to an initialize method on the wrapper, with
primitive arguments -- string literals, array literals, member references); the field declaration
itself is uniform `<type> <name>{};` (C++) or its LLVM IR equivalent (an alloca sized by the
type-mapping result, plus a `call` to the initialize method). Render never composes the wrapper's
name or constructor arguments from type payload.

When render needs to name a runtime library type to fabricate construction arguments, MIR is missing
the right primitive -- usually a way to express the per-member initialization as a call against the
wrapper's own API. The fix is to redesign the wrapper API so MIR can call it through existing
primitives (one initialize method per construction variant, taking flat primitive arguments), then
HIR-to-MIR emits the call, then render translates uniformly. The asymmetry between "some members are
default-initialized, some need construction" is expressed in MIR's expression set, not in render's
branch tables.

A literal of a runtime library type is not an exception to invariants 2 and 3, and reads like one
only until it is lowered. Such a value has no literal form in a target language at all -- it is a
runtime object -- so what stands for one in MIR is the factory that builds it, and HIR-to-MIR states
that factory the same way it states every other call. Which factory a constant needs follows from
its own bits, and reading them is a lowering decision made once, in one place, over data the
lowering already holds; a render that made it instead would be reading the same bits in every
backend to reach the same answer. What survives to render is an ordinary call.

A value-emission entry may not read _past_ its node to synthesize operands or pick a form: not an
enclosing declaration, not a sibling, not an expected destination, not a node reached by matching on
what an operand happens to be. Its own operands are a different matter and are named by invariant 1
as structural context -- an access asking whether the value it reaches is held through a pointer is
reading the input it was handed, not looking around.

The shape a conversion or factory call must hand the runtime therefore travels as an ordinary MIR
operand: a node that names the type whose shape it is, and whose own type is that type's runtime
descriptor. It carries no contents, because the width, signedness, state domain, and dimension stack
are the named type's. It reaches render as its own leaf node, never composed by the consuming call's
render from the call's type. A render branch that reads a call's type qualification to append shape
arguments is the forbidden shape; naming the type as an operand is the mechanical alternative.

A value of that type would say the same thing, and is the wrong way to say it: nothing downstream
can tell such an operand from one whose contents matter, so every backend builds contents it then
discards, and every runtime entry taking one advertises a parameter whose value it must document as
unused.
