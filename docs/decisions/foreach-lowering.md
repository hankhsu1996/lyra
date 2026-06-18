# `foreach` lowering shape

Date: 2026-06-02 (flat-counter); 2026-06-17 (revised to nested loops + labeled break; extended to
associative key-walk dimensions) Status: accepted

## Context

LRM 12.7.3 defines `foreach` uniformly over "any type of packed or unpacked array" with arbitrary
dimensionality, optional skipped dims, and optional trailing-dim omission. Two LRM 12.8 semantics
constrain the lowering:

- `continue` advances the iteration to the next combined `(i, j, k, ...)` tuple.
- `break` exits the **entire** foreach, not just the innermost dimension.

Two further requirements surfaced once variable-size arrays entered scope:

- A dynamically sized dimension's element count is known only at run time (LRM 7.5 / 7.10). For a
  **jagged** array (`int a[][]`) an inner dimension's length depends on the enclosing index --
  `a[i].size()` differs per `i` -- so the iteration space is not a rectangle.
- LRM 12.7.3 samples each bound once on entry to its dimension (the construct "is similar to a
  repeat-loop", and a repeat-loop evaluates its count once).

The hard design question is how to express "exit out of N enclosing loops" in the emitted C++. C++
has no labeled break -- the construct Java, JavaScript, Go, Rust, Kotlin, Swift, and Perl all
provide natively, and the natural primitive in LLVM IR and WebAssembly.

## Decision

**`foreach` lowers to one ordinary nested loop per iterated dimension, and a break that must leave
the whole foreach is modeled as a labeled break -- the universal multi-level-exit primitive.**

1. **Nested loops, one `for` per dimension.** The non-skipped dimensions become nested `for` loops
   in cardinal order (outer to inner). Each dimension supplies the `for`'s init / condition / step;
   that is the sole type-dependent step, and the three families all fit the one shape. A fixed
   dimension loops over its declared range and direction. A dynamically sized dimension samples its
   element count once into a synthetic local on entry to that level -- `a.size()` for the outermost
   dimension, `a[i].size()` for the next, and so on -- then counts `0..count-1`. Because the inner
   count is read inside the enclosing loop, a jagged array iterates correctly with no special case:
   the inner bound simply reads the current row. An associative dimension walks by key (LRM 7.9.4 --
   7.9.7) instead of counting an index: the `for`'s counter holds the first / next return, its step
   advances the key, and its loop variable is the key the body indexes by. `continue` lands on the
   step, so it advances to the next key (LRM 12.8); an empty array returns 0 from `first`, so the
   body never runs. Key-walked and index-counted dimensions nest freely in one foreach, since both
   are just a `for`.

2. **Labeled break.** HIR and MIR carry a `LoopLabelId`: a loop may be a break target, and a `break`
   may name the loop it exits. A `break` whose innermost SystemVerilog loop is the foreach carries
   the outermost loop's label; an ordinary innermost break carries none. The AST-to-HIR walk threads
   the current foreach label through the body and clears it on entering any nested loop, so a break
   binds to the nearest enclosing loop exactly as SystemVerilog requires. `continue` and `return`
   stay plain: a plain innermost `continue` already advances to the next tuple, and a plain `return`
   already exits the enclosing subroutine.

3. **Each backend renders the primitive its own way.** The C++ backend, lacking labeled break, emits
   a `goto` to a label placed after the outermost loop -- the canonical C idiom for leaving a loop
   nest. An LLVM backend would emit a branch to the loop's merge block directly. The labeled break
   lives in the semantic layers (HIR/MIR); the exit mechanism is a backend concern.

## History: why not a flat counter

The first implementation (2026-06-02) desugared every `foreach` to a **single** flattened loop: a
synthetic counter ran `0..total-1` over the Cartesian product of the dimension counts, and each loop
variable was recovered by decomposing the counter
(`var = base + sign * ((n / inner_product) % count)`). Its appeal was that a single loop makes
`break` / `continue` / `return` map one-to-one to the plain C++ statements with no labeled-exit
mechanism at all.

That shape has a fatal limitation: the flat counter assumes a **rectangular** iteration space (a
single `total` and per-dimension `inner_product`). It cannot represent a jagged array, where an
inner count depends on the outer index. The flat counter was, in effect, a workaround that avoided
modeling multi-level break by collapsing the nest -- and the collapse is only possible when every
dimension's bound is independent of the others. Once jagged dynamic arrays were in scope, the
workaround could not generalize, so the nested-loop model with an explicit labeled break replaced
it.

The earlier rejected alternatives to plain nested loops were re-examined and the
labeled-break-via-`goto` choice (item 3 above) is the resolution:

- **Nested loops + a `__break` flag** ANDed into every loop condition: correct but pays a
  per-iteration branch at every level and spreads the break logic across N+1 sites that must stay
  coherent. The single `goto` is simpler and costs nothing per iteration.
- **`goto` after the nest** was originally rejected purely on a project-wide "no goto" preference.
  With jagged support now required, that preference is the thing in tension, and a _generated_,
  localized `goto` -- the faithful rendering of a labeled break that no hand-written code ever sees
  -- is the right trade. The MIR models the labeled break; only the C++ backend spells it `goto`.
- **IIFE / lambda with `return` for break** corrupts SystemVerilog `return` from inside the body,
  which must exit the enclosing function, not the lambda.
- **`std::views::cartesian_product`** only models rectangular spaces and pushes the emitted-code
  toolchain floor higher; rejected for the same jagged limitation as the flat counter plus a
  dependency cost.

## Consequences

- The lowering is uniform across dimensionality, direction, skipped and trailing-omitted dims, and
  across fixed, dynamic, queue, and jagged element containers. Each dimension is an ordinary `for`;
  nothing about the foreach touches element access, which the body's existing index lowering
  handles.
- Per-iteration cost is a plain nested loop. A dynamic level pays one `.size()` read on entry
  (sampled into a local, not re-queried each iteration).
- The labeled break is the only new IR concept, and it is the same primitive every other
  multi-level-exit language and IR uses, so the model transfers directly to a future LLVM backend (a
  branch) rather than baking in a C++-specific trick.
- Associative-array `foreach` fits the same nested-`for` model with no new construct: its dimension
  is just a `for` whose pieces are a key walk instead of an index count. String `foreach` remains
  rejected: it iterates by byte (LRM 6.16), a distinct model with its own lowering.

## Cross-references

- LRM 12.7.3 (foreach syntax, dim cardinality, bound sampled once, read-only loop vars), 12.8 (break
  exits whole foreach; continue advances loop vars), 7.5 / 7.10 (dynamic array / queue runtime
  size).
- Multi-level-exit primitives surveyed: Java / JavaScript / Go / Rust / Kotlin / Swift labeled
  break, Perl `last LABEL`, LLVM IR branch to merge block, WebAssembly `br N`. C is the outlier with
  no labeled break; its idiom is the `goto` the C++ backend emits.
