# HIR

## Purpose

HIR is a SystemVerilog-faithful semantic IR. Its job is to mirror SV's language structure in a form
the compiler owns: declarations, expressions, statements, processes, assertions, functions, tasks,
parameters, ports, and generate constructs all appear as first-class HIR nodes carrying their
LRM-level shape. HIR is the compiler's first canonical representation and the cutover from frontend
identity to compiler identity -- slang's symbol ids are discarded at AST-to-HIR; HIR's typed ids
replace them with no loss of LRM-level information.

HIR is not a generic programming-language IR -- that role belongs to MIR. SV-specific concepts
(signals, non-blocking assignments and the deferred scheduling they imply, event-control semantics,
generate-as-elaboration, ports-as-aliases) all live here verbatim, awaiting translation into a
generic programming-language vocabulary at HIR-to-MIR. Loops are loops, assertions are assertions,
classes are classes; the abstraction level is the language the user wrote, not what the compiler
later turns it into.

HIR's role between AST and MIR mirrors the role a language's own semantic IR plays in other
compilers: a language-faithful representation the compiler owns, with frontend syntactic noise
removed and canonical compiler-owned identity in its place, but with the language's vocabulary still
visible. The translation away from that vocabulary is the next layer's job, not HIR's.

## Owns

- Language constructs as first-class nodes: declarations, expressions, statements, processes,
  assertions, functions, tasks, parameters, ports.
- Compilation-unit-local declarations with typed identities, one id kind per kind of declaration
  (variables, nets, parameters, instance members).
- The generate-region tree as a first-class structure inside each module compilation unit.
- The canonical association between a declaration and its references within the same compilation
  unit.
- Source locations for every construct that may produce a diagnostic.

## Does Not Own

- Control-flow graphs, basic blocks, or phi nodes.
- Execution scheduling, dirty tracking, or subscription wiring.
- Low-level storage layout or ABI decisions.
- Hierarchical paths beyond the compilation unit's own generate tree (see
  `hierarchy_and_generate.md`).
- A generic programming-language vocabulary. SV's vocabulary survives at HIR; the translation into a
  generic vocabulary is MIR's identity, performed at HIR-to-MIR.

## Core Invariants

Each invariant below is a direct consequence of the identity stated in Purpose -- HIR is the
compiler's first canonical, SV-faithful representation. An invariant that cannot be re-derived from
that identity is the suspect, not the analysis.

1. Every compilation-unit-local declaration has a typed id. References to that declaration carry the
   typed id, never a frontend symbol id. _Source-faithful consequence: HIR is the layer's source of
   identity; frontend identity ends at AST-to-HIR._
2. A reference that targets a declaration outside the current compilation unit uses a separate,
   explicitly named variant. Local and cross-unit references never share a key space. _Source-
   faithful consequence: SV's compilation-unit boundary is explicit in the IR, not flattened._
3. HIR preserves LRM-level constructs without flattening them into lower-level primitives. Loops are
   loops, assertions are assertions, classes are classes. _Source-faithful consequence: the
   abstraction level is the language the user wrote; flattening into a generic vocabulary is MIR's
   job._
4. Each compilation unit is self-contained: resolving any intra-unit reference requires no data
   outside that unit. _Source-faithful consequence: independent compilation is preserved at the
   layer where SV's unit boundary lives._
5. The generate tree lives inside the module compilation unit and is the sole representation of
   generate-region structure at HIR level. _Source-faithful consequence: SV's generate construct is
   carried verbatim, not pre-elaborated._
6. HIR is the canonical semantic identity of the compiler. No frontend id survives past AST-to-HIR.
   Downstream layers consume HIR identity, never frontend identity. _Source-faithful consequence:
   the cutover from frontend to compiler-owned identity happens at one boundary, not by gradual
   propagation._

## Boundary to Adjacent Layers

- Consumes slang's AST. AST-to-HIR is a cutover, not a translation: frontend ids are discarded at
  this boundary. HIR establishes the first canonical semantic identity owned by the compiler.
- Produces input to MIR. HIR-to-MIR is the layer where SV's vocabulary gets translated into a
  generic programming-language vocabulary. Every SV-specific concept that does not appear in MIR is
  translated at this boundary or rejected as unsupported. HIR-to-MIR holds the SV knowledge; MIR
  does not.

## Forbidden Shapes

These are the patterns that violate the identity. Each is the inverse of a property a source-
faithful, compiler-identity-owning IR implies.

- `SymbolId` or any frontend-global id used as the identity of a compilation-unit-local declaration
  reference. (HIR owns identity at this layer; frontend identity ends at AST-to-HIR.)
- A resolver keyed on `(instance_id, local_symbol)` at HIR level. (Per-instance identity belongs to
  the runtime object graph, not HIR.)
- CFG-shaped nodes (basic blocks, successor lists, phi nodes) anywhere in HIR. (HIR is structured-
  language faithful; CFG is LIR's vocabulary.)
- A generate-region representation that lives outside the compilation unit it belongs to. (Generate
  is SV-source structure inside a unit; lifting it out flattens the unit boundary.)
- Coordinate or ordinal identity for a generate region. (Identity is typed and carried by HIR's own
  id kinds.)
- Side-table reconstruction of declaration ownership from names or paths. (Ownership is structural,
  not name-based.)
- Dependence on a global lookup table to resolve a local reference. (Local references resolve inside
  the unit by structure.)
- Carrying frontend ids into HIR or any downstream layer. Frontend ids end at AST-to-HIR.

## Notes / Examples

A reference to a local variable carries a `VariableId` in the compilation unit's arena. A reference
to a package-level entity or a cross-unit target uses a distinct variant. The two are not
interchangeable and never share a key space.

A SystemVerilog `always_comb` block, a non-blocking assignment, an event control, and a `generate`
region all appear in HIR as themselves -- a process kind, an NBA statement, an event-control
expression, a generate node. HIR-to-MIR is where each gets translated into MIR's generic
programming-language vocabulary (a process becomes a callable invoked by the scheduler; an NBA
becomes a deferred closure submission; an event control becomes a coroutine suspension; a generate
becomes constructor-time logic). The SV vocabulary is HIR's; the generic vocabulary is MIR's.
