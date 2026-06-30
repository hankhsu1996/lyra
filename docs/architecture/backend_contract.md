# Backend Contract

## Purpose

Define the mechanical-translation contract a backend must satisfy when consuming per-unit MIR.

The architectural target of the compiler is HIR -> MIR -> LIR -> LLVM IR. The C++ backend is a
transitional realization: it consumes the same MIR but renders to C++ source instead of descending
through LIR and LLVM IR. The C++ backend's emitted output is **how MIR is observed during the
architecture reset** -- a developer reads the emitted C++ to validate that MIR's shape faithfully
represents the source SystemVerilog through MIR's semantic model (`compiler_overview.md`).

The transitional status does **not** loosen the mechanical-translation discipline. The contract in
this doc is exactly the discipline an eventual LLVM IR backend will need: every render rule is a
fixed function of one MIR node. If the C++ backend's render needs any decision logic -- an `if` on a
node's payload, a ternary inferring construction shape, a switch on type variant whose arms produce
different syntactic structure -- the LLVM IR backend will need the same logic in its target. Both
backends pay the cost; the cost is a MIR design failure visible from the backend side.

The C++ backend's render therefore serves as the cross-check on MIR shape today: any place where its
render is not a mechanical single-node translation is a place where the next stage (LIR / LLVM IR)
will hit the same obstruction. The bug is in MIR, not in render.

## Owns

- The principle that a backend render entry is a **fixed function of one MIR node**: input is the
  MIR node and its structural context (its children, the node consuming it); output is determined by
  that input alone. The recursion structure follows the MIR node tree; the render of each node
  composes target-language wrapping around recursive renders of its child nodes.
- The boundary between two backend entry kinds:
  - **Type mapping** -- one dispatch per MIR type variant returning a target representation (a
    target-language type literal for C++, a size + an LLVM type for LLVM IR). This is the only entry
    that names a target-language runtime library type.
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

2. **Any decision logic inside a value-emission entry is a MIR design failure.** An `if`-branch,
   ternary, or switch whose arms produce different syntactic shape in a value-emission entry is
   render making a semantic decision. Render does not decide; it translates. If a render needs to
   branch, MIR is not stating something the program needs to say, and HIR-to-MIR's output is leaving
   render to fabricate it. The fix is upstream: extend MIR to state the missing semantic explicitly,
   then render becomes uniform.

3. **Type mapping is the only entry that names a runtime library type literal.** Every MIR type
   variant maps to a target-language type representation through one dispatch. Value emission
   entries render types only via that dispatch; they never compose a target-language type literal
   directly. A runtime library type's spelling lives at one place; nowhere else.

4. **Member declaration is (name, type) -- nothing else reaches member render.** A member's
   target-language declaration form is determined by its name and its type alone (the type carries
   size, offset, and target type form; the name carries the source identifier). Wrapper-typed
   members are no exception: any per-member construction state arrives later as ordinary MIR
   expressions in the constructor body, never as type payload that member render reads.

5. **The LLVM IR backend is the canonical cross-check.** When the C++ backend's render needs an
   `if`-branch or a payload-driven shape decision, ask: could a mechanical LLVM IR backend translate
   the same MIR node without that decision? If not, the MIR shape is wrong. The C++ backend's
   transitional status does not relax this check; it sharpens it, because the C++ backend's output
   is how MIR's correctness is currently observed.

6. **The set of backends consuming MIR is open.** No MIR primitive and no contract entry is
   specialized for one backend. A new backend reads the same MIR; the only thing it brings is its
   own type-mapping dispatch and value-emission rules for its target's syntactic form.

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

- **An `if`-branch, ternary, or switch in a value-emission entry whose arms produce different
  syntactic shapes.** This is the canonical render-side defect: render is making a decision that
  should be a MIR-level distinction. The fix is upstream, never inside render. (The sole exception
  is a leaf literal materializing its own value -- see Notes.)

- A value-emission entry that composes a target-language type literal as a string. Every
  target-language type literal a backend emits comes from the type-mapping dispatch.

- A value-emission entry that names a runtime library identifier (wrapper type, helper function,
  helper struct, method spelling) directly. The runtime library's identifiers appear at render
  through the MIR types and MIR calls that map onto them. (The sole exception is a leaf literal
  materializing its own value -- see Notes.)

- A value-emission entry that switches on a MIR type's payload to choose a non-mechanical emission
  form. Type payload is for type-mapping; value emission acting on it reads value-layer data carried
  in a type.

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

A read of an observable signal is a `CallExpr` whose callee is the wrapper type's read method
against the receiver expression that reaches the wrapper-typed field. The backend renders this
`CallExpr` by rendering the callee, the receiver, the argument list, and the call syntax of its
target form. The wrapper-type spelling reaches the call only through the type-mapping dispatch on
the receiver's type; the call render itself does not separately know the wrapper's name.

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

A leaf literal is the one bounded exception to invariants 2 and 3. An integer, real, or string
literal has no faithful render but a runtime-library construction of its own value, so its render
names the runtime value type and composes the construction -- including choosing the constructor
form that expresses the literal's own value kind, such as a narrow integer versus a wide or
X-bearing one -- from that literal's value and type alone. The exception is bounded to a literal
materializing _itself_: it earns the carve-out only because the construction is a fixed function of
the single literal node. A value-emission entry may not read another node's type -- an enclosing
declaration, a call qualification, an expected destination -- to synthesize operands or pick a form;
that is the forbidden shape, with no exception.

A shape that a conversion or factory call must hand the runtime therefore travels as an ordinary MIR
value: a literal of the destination type, whose contents are unused and whose type supplies the
shape. The shape reaches render as its own leaf node (rendered by the exception above), never
composed by the consuming call's render from the call's type. A render branch that reads a call's
type qualification to append shape arguments is the forbidden shape; the prototype-value argument is
the mechanical alternative.
