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
- [x] C4 -- `while` (procedural while-loop). Reproduces every `control_flow/while/default.yaml` case
      that does not depend on `break` (deferred to C7). Includes runtime-condition loops, countdown,
      `while (0)` zero iterations, two-level nesting, and mixed nesting with `for` in both
      directions.
- [x] C5 -- `repeat (N)`. Reproduces every `control_flow/repeat/default.yaml` case that does not
      depend on `break` (deferred to C7). HIR carries a faithful `RepeatStmt`; HIR -> MIR desugars
      to a wrapper `BlockStmt` containing a count snapshot (preserves SV's "evaluate count once"
      semantic for side-effecting count expressions) plus an `mir::ForStmt` over the C2 machinery.
      Coverage includes literal counts, runtime counts, zero iterations, count-evaluated-once,
      nesting, single-statement body, and mixed nesting with `if`/`for`/`while`.
- [x] C6 -- `do_while`. Reproduces every `control_flow/do_while/default.yaml` case that does not
      depend on `break`/`continue` (deferred to C7). HIR carries a faithful `DoWhileStmt`; MIR
      mirrors it with the condition stored in the enclosing procedural scope and the body in a child
      scope (same shape as `WhileStmt`). The C++ backend renders directly to
      `do { body } while (cond);`. Coverage includes basic loops, `do {...} while (0)` (body still
      executes once), runtime conditions, nesting, single-statement body, and mixed nesting with
      `if`/`for`/`while`.
- [x] C7 -- Loop control: `break`, `continue`. New HIR/MIR statements (no payload) lowered as
      passthrough to the C++ backend, which renders `break;` and `continue;` directly. C++ scoping
      already exits only the innermost surrounding loop, which matches SV semantics for every loop
      shape we model -- including `repeat (N)`, where break/continue land inside the desugared inner
      `for` (the surrounding count-snapshot block is not a loop). Coverage includes `for_break`,
      `for_continue`, `for_break_inner_only`, `while_break`, `while_continue`, `do_while_break`,
      `do_while_continue`, `repeat_break`, `repeat_continue`, and a mixed `break + continue` case.
      Coroutine interaction (early-exit across `co_await` inside loop bodies) is not exercised
      because timed bodies in loops are not yet supported; revisit when event-control loops land.
- [x] C8 -- `forever`. HIR carries a faithful `ForeverStmt` (mirrors the other loop nodes); HIR ->
      MIR desugars to `mir::ForStmt` with no init, no condition, and no step. The C++ backend
      renders this as `for (; ; ) { body }`, which loops until `break` or `$finish` exits. Coverage
      includes `$finish` termination, `break` termination, `continue`, single-statement body (no
      `begin`/`end`), nested forever with inner `break`, and `if`-guarded forever.
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
