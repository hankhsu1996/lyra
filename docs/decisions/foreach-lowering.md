# `foreach` lowering shape

Date: 2026-06-02 Status: accepted

## Context

LRM 12.7.3 defines `foreach` uniformly over "any type of packed or unpacked array" with arbitrary
dimensionality, optional skipped dims, and optional trailing-dim omission. LRM 12.8 adds two
semantics that are critical to the lowering choice:

- `continue` jumps to the end of the loop for the **current set of loop variable values**, i.e.
  advances the iteration to the next combined `(i, j, k, ...)` tuple.
- `break` jumps out of the **entire foreach**, not just the innermost dimension.

The pre-reset archived implementation desugared `foreach (arr[i, j])` to N nested for-loops
(`for (int i = ...) for (int j = ...) BODY`) and let plain SV `break` lower to plain C++ `break`.
That shape is structurally close to the SV source but is subtly wrong on LRM 12.8: a `break` inside
the body terminates only the inner `for`, after which the outer `for` continues to its next
iteration. None of the archived test cases happened to expose the gap, but it is a real LRM
correctness defect.

This is also a forcing function on a broader design choice: how do we express "non-local exit out of
N enclosing scopes" in the emitted C++? C++ has no first-class labeled break (the construct in Java
/ Rust / Kotlin and the natural primitive in LLVM IR / WebAssembly).

## Decision

1. **Every `foreach` lowers to a single flattened `hir::ForStmt`.** The iteration space is the
   Cartesian product of the non-skipped dimensions. A synthetic procedural variable
   `__lyra_foreach_n` is declared in the loop's `ForInitDecl` and counts from `0` to `total - 1`,
   where `total` is the product of the non-skipped dim counts. The loop body begins by computing
   each loop variable's per-iteration value by decomposing the counter, then continues with the
   lowered user body. There is exactly one for-loop level in the lowered HIR regardless of
   dimensionality.

2. **Per-dim decompose formula.** Each non-skipped loop variable is computed as

   ```
   var = base + sign * ((__n / inner_product) % count)
   ```

   where `inner_product` is the product of all non-skipped dim counts to its right (1 for the
   innermost), `count = abs(range.right - range.left) + 1`, and `(base, sign) = (range.left, +1)`
   for an ascending range, `(range.left, -1)` for a descending range. Slang's `LoopDim` order is
   already outer->inner per LRM cardinality, so no reordering is needed.

3. **Plain SV `break` / `continue` / `return` lower to plain HIR `BreakStmt` / `ContinueStmt` /
   `ReturnStmt`, with no foreach-aware rewriting.** Because the lowered HIR has exactly one
   for-loop, plain `break` exits the entire foreach (matching LRM 12.8), plain `continue` advances
   `__n` to the next tuple (matching LRM 12.8 "current set of loop variable values"), and `return`
   exits the enclosing subroutine without any wrapper interception. This is the design's payoff: no
   special primitive, no flag, no rewrite, no lint exception.

_Rejected alternatives:_

- **Nested for-loops + plain `break`** (archived shape): LRM-incorrect for break -- terminates only
  the innermost dim.

- **Nested for-loops + flag-based break propagation**: each outer loop tail checks a synthetic
  `__break` flag and re-breaks if set; the body's `break` becomes `__break = 1; break;`. Correct but
  adds N+1 places per dim that must stay coherent across edits, and pays a per-iteration branch cost
  at every level. Mechanism does not collapse with dim count.

- **Nested for-loops + `goto label;` after the outermost closing brace**: matches structure 1:1 and
  is the natural emit for "break to labeled block" in C++. Rejected because a single `goto`
  exception in emitted code undermines a clean project-wide rule against `goto` and invites future
  misuse (anywhere else where someone wants a "controlled" jump). The cost of the rule violation
  outweighs the structural fidelity gain.

- **Nested for-loops wrapped in an IIFE; SV `break` -> C++ `return`**: cleanly handles `break` by
  returning from the lambda, but corrupts SV `return` from inside the foreach body -- the C++
  `return` exits the lambda, not the enclosing SV function. Modeling the difference would require
  separating "break-style return" from "function-style return" at every emit site, defeating the
  simplicity.

- **`std::views::cartesian_product` (C++23 ranges)**: conceptually correct -- analogous to Python's
  `itertools.product`. Rejected because `cartesian_product` is a libc++ 19+ (Sept 2024) / libstdc++
  13+ stdlib feature; requiring it pushes the toolchain floor for the emitted C++ beyond what the
  project will commit to (see `docs/progress/refactor.md` R5 for the toolchain-baseline workstream).
  The same _semantic_ -- flattening the product space -- is captured by the manual flat-index
  decompose without any stdlib dependency.

## Consequences

- The lowering is **uniform across dimensionality**. 1D / 2D / N-D / mixed packed-unpacked /
  ascending / descending / skipped-dim cases share one code path. The dispatch over packed vs
  unpacked element access happens entirely in the body's existing `ElementSelectExpr` lowering,
  which the foreach itself never touches.

- Per-iteration cost is **N-1 divisions and N modulos on `__n`** for an N-dim foreach. All divisors
  are compile-time constants for fixed-bound arrays (always the case in C9's scope), so modern C++
  compilers fold them to multiply-by-magic-number sequences (~2-3 instructions per dim). The emitted
  C++ for-loop has no flag overhead and no lambda boundary, so the only excess vs nested for-loops
  is the index arithmetic. `foreach` is not a typical simulator hot path.

- Skipped dimensions (`arr[i, , k]`) and trailing-dim omission (`int arr[2][3]; foreach (arr[i])`)
  fall out of the same model: skipped dims contribute neither a loop variable nor a factor to the
  product, and the body simply never references them. No special-case code paths required.

- The lowering reaches the body via the usual recursive `LowerStatement`, so `break` inside a `for`
  / `while` / `do-while` nested in the foreach body still targets the nested loop (standard C
  nesting). The "exit the entire foreach" semantic is structural -- there is only one for-loop --
  not contextual.

- Dynamic-array / queue / associative-array element types are rejected with
  `kUnsupportedStatementForm` until `datatypes/general` brings procedural support for those types.
  When that lands, the same flat-index model extends naturally: the iteration count becomes a
  runtime query (`arr.size()`) instead of a compile-time literal, and the decompose formula stays
  identical.

- The slang `IteratorSymbol` for a foreach loop variable derives from `VariableSymbol` but carries
  its own `SymbolKind::Iterator`. The AST -> HIR name resolution at
  `src/lyra/lowering/ast_to_hir/expression/lower.cpp` was extended to accept `Iterator` alongside
  `Variable` and `FormalArgument`; all three route through
  `ProcessLoweringState::LookupProceduralVar`.

## Cross-references

- LRM 12.7.3 (foreach syntax, dim cardinality, implicit begin-end, read-only loop vars).
- LRM 12.8 (break exits whole foreach; continue advances current loop var values).
- Conceptual analogue: Python `itertools.product`, C++23 `std::views::cartesian_product`,
  WebAssembly `br N`, LLVM IR labeled blocks. None used directly; all express the same
  flatten-the-product-space idea.
- `docs/progress/refactor.md` R5 -- emit-CPP smoke suite and toolchain baseline (the reason
  `cartesian_product` was rejected).
