# Lowering Organization

## Purpose

Define how a lowering or rendering pass is structured: the class shape, the walker method signature,
the four kinds of fields a class member may be, and the rules that keep new concepts cheap to add
and the class bounded.

## Owns

- The pattern that every lowering or rendering pass is implemented as a class scoped to one task
  instance (one process, one scope, one unit, etc.).
- The classification of every class member into one of four kinds: facts, registries, builders, and
  walk-frame references.
- The walker method signature shape: two parameters (the node, the walk frame) and a
  `diag::Result<T>` return when the method can emit a diagnostic.
- The constructor convention: facts are injected at construction; registries and builders are owned
  by the class.
- The rule that the pattern is shared across lowering and rendering passes while concrete types are
  per pass.

## Does Not Own

- The concrete data carried by any one pass's class.
- IR layer shape (`hir.md`, `mir.md`, `lir.md`).
- Lowering permissions (`lowering_boundaries.md`).

## Core Invariants

1. Every lowering or rendering pass is implemented as a class named for the unit of work it
   processes and the action it performs (`ProcessLowerer`, `CppProcessRenderer`, `ScopeLowerer`).
   The class is constructed once per task instance and destroyed when that task completes; one class
   instance does not span multiple task instances.

2. The class constructor receives the read-only facts the pass depends on. After construction those
   facts are not mutated. Registries and builders the pass accumulates are class members owned by
   the class instance, with lifetimes equal to the class instance's lifetime.

3. The class exposes one public entry method (typically `Run`) that performs the task and returns
   its output. Internal walker methods are private and access class state through `this`.

4. Walker method signatures are exactly
   `(const NodeType& node, WalkFrame frame) -> diag::Result<OutputType>` (or the raw `OutputType`
   when the method cannot emit a diagnostic). Walker methods do not receive per-pass state as
   separate parameters; per-pass state lives on the class.

5. Every class member is exactly one of four kinds: **facts** (set at construction, never mutated
   thereafter), **registries** (append-only accumulators whose API exposes `Add` and `Lookup`),
   **builders** (mutable scope-construction objects), or **walk-frame references** (references to
   per-task objects that the walk frame points at, owned by the class for lifetime reasons).

6. `WalkFrame` is a small value type holding per-recursion traversal context: a reference to the
   current write-target builder, the current procedural depth, an optional closure context, and any
   future stack-discipline state. It is passed by value between walker methods. Each recursion may
   construct a new frame with different fields set; the cost of copying is trivial.

7. Adding a new fact is a class-member addition and a constructor-parameter addition. It does not
   change walker method signatures. Adding a new traversal-time concept is a `WalkFrame`
   struct-field addition. It does not change walker method signatures and it does not add a class
   member. This rule is the contract's load-bearing scalability promise.

8. The `const` qualifier marks a member's access pattern, not the class. A `*State` suffix does not
   appear on any pass class, any member type, or any field name. Type names describe what the type
   represents.

9. The pattern (class plus walker methods plus `WalkFrame`) is identical across lowering and
   rendering passes. Concrete types (`ProcessLowerer`, `CppProcessRenderer`) are per pass; no base
   type spans lowering or rendering pass boundaries.

10. A pass class's per-kind dispatch is centralised. When a walker method dispatches by node kind
    (expression kind, statement kind, etc.), exactly one switch in one file owns the kind-to-handler
    routing. Per-kind handlers are free functions that take the relevant pass class instance by
    reference plus the walk frame plus the node. New kinds are added to the switch and to a
    subsystem file; the pass class definition is not modified.

11. Per-kind handlers are grouped by semantic family (operators, references, calls, selects,
    aggregates, etc.) and live in subsystem header / implementation pairs alongside the central
    dispatcher. The boundaries of a subsystem are determined by LRM-level expression / statement
    family, not by individual node kind. A subsystem header declares the handlers it owns; a
    subsystem implementation defines them. The central dispatcher includes every subsystem header to
    wire the switch.

12. A central dispatch contract header (typically named `dispatch.hpp` within the subsystem folder)
    declares only the recursive dispatcher entries (`LowerProcExpr` / `LowerStructuralExpr` or
    equivalent). Subsystem implementations include this header to recurse into the dispatcher.
    Shared utilities and subsystem-internal helpers do not belong in the dispatch header; they have
    clearer homes (subsystem-internal helpers in the subsystem header; operations on lowerer or
    module state in the corresponding class).

## Boundary to Adjacent Layers

- A pass class is a layer-internal artifact. It is not exposed across lowering or rendering
  boundaries. The output of the pass (the constructed IR, the emitted source text) is the contract
  surface visible to downstream consumers.
- The class instance's lifetime is bounded by one task. The output is finalized in `Run` and
  returned by value; the class is destroyed.

## Forbidden Shapes

- A pass implemented as free functions threading a context-like object as a separate parameter.
- A `RenderContext`, `LoweringContext`, or any other `*Context` type used as the carrier for facts,
  builders, or walk-frame state. The name is an open invitation to unbounded growth.
- A class instance shared across multiple task instances (one `ProcessLowerer` reused for several
  processes).
- A class member whose lifetime exceeds the class's task instance.
- A walker method signature that takes per-pass state as a separate parameter alongside the
  `WalkFrame`.
- An immutable-context-with-`With*()` copy-on-descend pattern. The pattern's superficial appeal is
  symmetry with the walk frame; its cost is the Context name's open invitation, the `mutable` escape
  hatch required for accumulating state, and the parallel structure needed for the class that owns
  the accumulators.
- A walker struct passed as the single fat parameter. Same pathology as a context, with a different
  name.
- A `*State` suffix on a class, a member, or a field.
- A class member representing traversal state. Traversal state belongs to `WalkFrame`.
- An ambient setter / getter pair on the class whose only purpose is install / restore of a
  walk-position concept.
- A shared base class spanning lowering and rendering pass classes. The pattern is shared; types are
  per pass.
- A walker method that mutates the class's fact members. Facts are mutated only by the constructor.
- A bundle of multiple kinds (facts + registry + builder) into one type. Each member of a class is a
  single-kind object; a bundle that mixes kinds is a god object regardless of size.
- A central dispatch header that accumulates utilities beyond the recursive dispatcher entries. The
  header is restricted to dispatcher declarations; helpers used by exactly one subsystem live in
  that subsystem's header; helpers used by multiple subsystems either become methods on the owning
  class or are inlined at the call site. The dispatch header is not a junk drawer.
- Per-kind handlers expressed as methods on the pass class. Methods grow the public class
  declaration and force every translation unit that includes the class header to recompile when a
  kind is added. The per-kind layer is free functions in subsystem files; the pass class exposes
  only the four kinds of members and the walker entry methods.
- Per-kind handlers requiring narrowed access (separate facts / registry / builder references in the
  signature). The pass class instance is the single access pass for the task; narrowing the
  signature reproduces the sig-explosion shape this contract exists to escape.

## Multi-File Organization Within a Pass Layer

When a layer's expression / statement dispatch grows past a single readable file, the layer is split
into a central dispatcher file and one subsystem file per expression / statement family. The shape
is fixed:

- One `dispatch.hpp` header in the layer's folder declares the recursive dispatcher entries (the
  free functions the pass class's walker methods delegate to, e.g. `LowerProcExpr` /
  `LowerStructuralExpr`). It declares nothing else.
- One subsystem header per family declares the per-kind handlers for that family (`operators.hpp`
  for unary / binary / conditional / conversion; `calls.hpp` for the call expression; `selects.hpp`
  for element-select / range-select / member-access; `aggregates.hpp` for concat / replication /
  assignment-pattern variants; `references.hpp` for name resolution; `assignment.hpp` for assignment
  / increment-decrement and their assignability validation; `inside.hpp` for the inside operator;
  analogous families on the statement side).
- One implementation `.cpp` per header defines the family's handlers and any anonymous-namespace
  helpers private to the family.
- The central dispatcher `.cpp` (typically `lower.cpp` in the folder) includes every subsystem
  header, defines the dispatcher switch, defines the pass class's walker method wrappers, and
  contains only the literal-maker helpers and trivial trampolines.

Adding a new expression / statement kind touches three files: the subsystem header that owns its
family, the subsystem implementation, and the central dispatcher's switch. Other subsystems are
unaffected and do not recompile. Adding a new family creates a new header / implementation pair and
adds the corresponding switch cases in the central dispatcher; existing families are not touched.

The pass class's public method count is bounded by the four-kind rule and the contract's promise
that walker signatures do not grow with new concepts. Per-kind handlers are not methods on the pass
class.

## Notes

The four kinds of class members correspond to distinct access patterns:

- **Facts** are constructor inputs. They are referenced from member functions through `this` and are
  never mutated post-construction. Examples: an injected type-lookup table, an injected builtins
  struct, an injected HIR module reference, an injected time resolution.
- **Registries** accumulate append-only entries. Their API exposes `Add` and `Lookup`; the contract
  is enforced by the registry type's surface, not by external `const` qualifiers. Examples: a
  procedural-var binding table, a static-locals list, a HIR-to-MIR type-translation map.
- **Builders** accumulate the output IR for one scope. One builder per output scope. The class
  instance owns the per-task root builder; nested-scope builders are constructed on the stack and
  referenced through the walk frame.
- **Walk-frame references** are member fields holding objects that the walk frame needs to point at
  (e.g., the per-task root scope builder). They are owned by the class for lifetime reasons; the
  walk frame carries the reference.

Entry points outside the pass (e.g., a driver that constructs one `ProcessLowerer` per process)
construct the class with its facts, invoke `Run`, and consume the returned IR or text. They do not
see the class's internal walkers.
