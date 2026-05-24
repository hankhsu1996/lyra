# Control Flow

Tracks procedural control-flow constructs against `backend::cpp`. Covers archive items under
`archived/tests/sv_features/control_flow/`. Each archive item checkbox in `architecture-reset.md` is
checked when its `*.yaml` cases reproduce on the current pipeline.

## Sub-Steps

### Statements

- [x] C1 -- `if`/`else` (procedural conditional). Reproduces
      `control_flow/conditional/default.yaml`. Multi-condition lists, patterns, and
      `unique`/`priority` qualifiers are rejected with `diag::Unsupported`; the qualifier work lives
      under `unique_priority_if` (see C13).
- [x] C2 -- `for` (procedural for-loop, no break/continue). Reproduces every
      `control_flow/for/default.yaml` case except the two `break`-bearing ones (deferred to C7).
      Includes inline `for (int i = 0; ...)`, external `for (i = 0; ...)`, multi-init / multi-step,
      countdown, zero-iteration, and `if`-in-body composition.
- [x] C3 -- `case` (plain, normal condition only) with literal labels. Reproduces the core
      plain-case subset of `control_flow/case/default.yaml` (basic match, default, multi-label,
      no-default-no-match, empty body, nested) using integer-literal labels and integer selectors.
      Variants requiring an if/else-cascade lowering (non-constant labels, duplicate labels) are
      tracked in C15; enum/string selectors and labels are tracked in C16 and C17 respectively.
- [ ] C4 -- `while` (procedural while-loop). `mir::WhileStmt` exists but
      `src/lyra/backend/cpp/render_stmt.cpp:268-271` throws `InternalError`; needs HIR node, AST ->
      HIR arm, HIR -> MIR arm, and render. Reproduces `control_flow/while/default.yaml` once C7
      lands.
- [ ] C5 -- `repeat (N)`. Pure desugaring to `for (int __k = 0; __k < N; __k = __k + 1)` over the C2
      machinery. Counts as one PR after C2 land.
- [ ] C6 -- `do_while`. Lowers to `while(true) { body; if (!cond) break; }` over C4 + C7.
- [ ] C7 -- Loop control: `break`, `continue`. New HIR/MIR statements + C++ render. Shared by C2
      (for) / C4 (while) / C5 (repeat) / C6 (do-while) / C9 (foreach). Coroutine renderer must scope
      the early-exit correctly inside nested control flow.
- [ ] C8 -- `forever`. Lower to `for (;;) { body }` (already representable via `mir::ForStmt` with
      empty condition); needs `$finish` runtime support for the archive tests to terminate.
- [ ] C9 -- `foreach` over fixed unpacked 1D arrays. Desugar to nested `for` over slang `loopDims`;
      depends on C2.
- [ ] C10 -- `foreach` multi-dim, skipped dimensions, dynamic-array, queue. Depends on C9 plus
      runtime types for the dynamic variants.
- [ ] C11 -- `casez` / `casex`. Cannot share `mir::SwitchStmt` (C++ `switch` does not support
      wildcard match); lower to an if/else cascade with masked comparisons in HIR -> MIR.
- [ ] C12 -- `case (... inside ...)`. Range patterns (`[lo:hi]`) and `inside` membership; lower to
      if/else cascade with `inside` semantics.
- [ ] C13 -- `unique` / `unique0` / `priority` qualifiers on `if` and `case`. Needs runtime warn
      helpers and (for cascaded `if`) settle-epoch tracking. Covers archive items
      `unique_priority_if` and `unique_priority_case`.
- [ ] C15 -- `case` with labels that are not compile-time constants (`case (sel) some_expr: ...`)
      and `case` with duplicate labels ("first match wins"). Neither can be expressed via C++
      `switch`; lower in HIR -> MIR to an if/else-if cascade. Covers archive cases
      `case_constant_expression`, `case_first_match_wins`.
- [ ] C16 -- `case` with enum-typed selectors and enum-value labels. Depends on the `datatypes/enum`
      archive item landing.
- [ ] C17 -- `case` with string selector / string labels. Needs the string-equality runtime helper
      from `operators/binary_string`.

### Expressions

- [ ] C14 -- Ternary `cond ? a : b`. Pure expression node; independent of statement work. Adds
      `hir::ConditionalExpr` + `mir::ConditionalExpr` and a one-line C++ render.

## Out of Scope

- `fork` / `join_any` / `join_none` -- scheduling primitives tracked under `processes`.
- `wait` / `wait_event` / `@(event)` -- event control, not pure control flow.
- Assertion control statements (`disable`, `restart`) -- tracked under `assertions`.
