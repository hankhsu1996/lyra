# North Star

## Purpose

Define the top-level design principles of the compiler. These are the non-negotiable objectives and
first-class constraints that shape every lower-level architecture document. If a lower-level
document conflicts with a North Star principle, the lower-level document is wrong and must be fixed.

## Owns

- The primary optimization target of the compiler as a whole system.
- The first-class constraints that every other architecture document must obey.

## Does Not Own

- Mechanism-level rules (identity schemes, IR boundaries, specialization keys, frontend boundaries).
  Those live in the lower-level documents and are constrained by, but are not the content of, North
  Star.
- Per-layer contracts. Each IR layer's shape is defined in its own doc (`hir.md`, `mir.md`,
  `lir.md`).
- Implementation strategy. North Star constrains design, not code.
- Operational processes (testing workflow, build commands, editor setup).

## Core Invariants

1. **End-to-end iteration time is the primary optimization target.** The compiler optimizes the full
   edit-compile-run-inspect loop, not simulation throughput alone. A design that cuts one stage of
   the loop at the cost of inflating another beyond budget is rejected.
2. **Compile per unit; elaborate at runtime.** The compile-time scope is the compilation unit
   (module, package, interface), not the elaborated instance graph. Generate constructs (`if`,
   `for`, `case`) are constructor-time logic the runtime executes to build the object graph at time
   zero. Compile-time artifacts are class-level and shared across every instance; per-instance data
   (parameter values, wiring, hierarchical position) flows in at runtime construction. Compile-time
   work scales with the count of distinct unit specializations, not with instance count.
3. **Correctness is independent of optimization.** The compiler produces correct output for every
   accepted program; the per-unit sharing of invariant 2 is admitted only when behavior-preserving,
   and a correct program is never rejected because an optimization cannot be applied.
4. **Incremental and parallel compilation are first-class architectural constraints.** The
   architecture is shaped from the start to support partial recompilation and concurrent compilation
   of independent work. These are not optimizations added later; they constrain every lower-level
   decision.
5. **The compiler is organized around independently compilable units with explicit dependencies.**
   Work decomposes into units that compile in isolation. Every cross-unit interaction is an
   explicitly declared dependency. Implicit shared state between units is rejected.

## Boundary to Adjacent Layers

- Every architecture document under `architecture/` must conform to these principles. A conflict is
  resolved against the lower-level document, not against North Star.
- Entries under `decisions/` may record trade-offs inside the space North Star leaves open, but may
  not override a North Star invariant.

## Forbidden Shapes

- A design that improves one stage of the iteration loop at the cost of inflating another beyond the
  overall turnaround budget.
- Letting per-instance compile-time work become the steady state, or foreclosing the per-unit shared
  form.
- Making the frontend-elaborated instance graph the compilation model, or the authority for unit or
  specialization identity.
- Making an optimization a correctness precondition: rejecting or mis-lowering a correct program
  because it cannot be shared.
- Hidden or implicit cross-unit dependencies that break incremental or parallel compilation.
- Architectural choices that force global serialization of otherwise independent compilation work.
- Implicit shared semantic state between compilation units.
- Undeclared cross-unit relationships that must be discovered at compile time rather than declared
  up front.

## Notes / Examples

Lower-level architecture documents define the mechanisms used to meet these principles.
Specialization policy, identity schemes, IR boundaries, and frontend boundaries belong in those
documents, not here. If a principle can be stated as a mechanism ("identity follows ownership",
"frontend pointer identity stops at AST-to-HIR"), it is not North Star material. North Star names
the objective and the first-class constraints; lower-level docs name the mechanisms.
