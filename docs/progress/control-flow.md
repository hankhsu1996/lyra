# Control Flow

Tracks procedural control-flow constructs against `backend::cpp`. Covers archive items under
`archived/tests/sv_features/control_flow/`. Each archive item checkbox in `architecture-reset.md` is
checked when its `*.yaml` cases reproduce on the current pipeline.

The numeric IDs (C1..C17) are stable references to archive items and do **not** imply execution
order. Most of the remaining open items depend on workstreams outside control-flow (data types,
operator runtime, string runtime); see the per-item **Depends on** lines and the
[Actionable](#actionable) summary below for what can actually be picked up next.

## Actionable

C11 is unblocked now that W2 has landed. C9 and C10 still wait on `datatypes/unpacked`.

| Item    | Status                                                                               |
| ------- | ------------------------------------------------------------------------------------ |
| C11     | Unblocked (W2 shipped). HIR -> MIR cascade with `==?` per-label compare.             |
| C9, C10 | Blocked on `datatypes/unpacked` (procedural unpacked array vars + `arr[i]` element). |

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
- [x] C3 -- `case` (plain, normal condition). HIR keeps a faithful `CaseStmt`; HIR -> MIR always
      desugars to an if/else-if cascade built from existing primitives (a `BlockStmt` wrapper that
      snapshots the selector into `_lyra_case_sel` for LRM 12.5 "evaluated once" semantics, then a
      nested chain of `IfStmt`s whose conditions compare the snapshot against each item's label list
      joined by `||`). `mir::SwitchStmt` no longer exists -- the cascade form subsumes both
      literal-label cases and the non-constant/duplicate-label variants tracked under C15.
      Reproduces the core plain-case subset of `control_flow/case/default.yaml` (basic match,
      default, multi-label, no-default-no-match, empty body, nested) using integer-literal labels
      and integer selectors. Enum/string selectors and labels are tracked in C16 and C17.
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
- [ ] C9 -- `foreach` over fixed unpacked 1D arrays. Desugar in HIR -> MIR to nested `for` over the
      slang `loopDims`. **Depends on** `datatypes/unpacked`: needs a procedural unpacked-array
      variable shape and an `arr[i]` element-select expression in both HIR and MIR (neither exists
      today). Cannot start before that work.
- [ ] C10 -- `foreach` multi-dim, skipped dimensions, dynamic-array, queue. **Depends on** C9 plus
      `datatypes/general` (dynamic array, queue) and the `.size()` runtime query.
- [ ] C11 -- `casez` / `casex`. Lower to an if/else cascade with masked comparisons in HIR -> MIR
      (C++ `switch` does not support wildcard match). **Depends on** `operators.md` W2 for the `==?`
      / `!=?` runtime helper.
- [x] C12 -- `case (... inside ...)`. Sibling statement family in HIR (distinct from plain case
      because labels are `range_list` entries -- value or `[lo:hi]` range -- and per-item match uses
      inside-membership rather than `==`). HIR -> MIR snapshots the selector (LRM 12.5
      evaluate-once) and builds the per-item inside-membership predicate against the snapshot using
      the same primitive the `inside` operator uses (asymmetric wildcard equality for value items,
      `(>= lo) && (<= hi)` for ranges, LRM 11.4.13 OR-reduction across items). The cascade's
      truthiness test already excludes `1'bx` per LRM 12.5.4 ("match only when inside returns
      `1'b1`"); no explicit 2-state clamp needed. Coverage: `case_inside_basic`,
      `case_inside_range`, `case_inside_mixed`, `case_inside_no_match`, and `case_inside_wildcard`
      (RHS wildcard `4'b00??` against a 4-state logic selector).
- [x] C13 -- `unique` / `unique0` / `priority` qualifiers on `if` and `case`. HIR carries the
      qualifier as an optional `UniquePriorityCheck` field on `IfStmt` / `CaseStmt`. HIR -> MIR
      desugars the site into a `BlockStmt` that snapshots each branch's predicate into a fresh
      procvar, hands a closure to the Observed region via `RuntimeSubmitObservedCall`, and renders
      the original cascade as a plain `if`/`else if` chain reading the snapshots. The closure body
      counts truthy snapshots and emits a warning through the existing `RuntimeDiagnosticCall` /
      `LyraDiagnostic` path when the qualifier-specific predicate fires (`unique`: count != 1;
      `unique0`: count > 1; `priority`: count == 0). The runtime drains `Module::observed_pending_`
      per-object during `Engine::ExecuteObservedRegion`; re-submits at the same site within one time
      slot collapse via vector-slot replacement, suppressing glitch-driven false positives. Covers
      archive items `unique_priority_if` and `unique_priority_case`. Source-location propagation
      still feeds `origin = nullopt`; the warning text reports the violation kind and the matched
      count but not the file/line of the offending statement -- to be plumbed in a follow-up.
- [x] C15 -- `case` with labels that are not compile-time constants (`case (sel) some_expr: ...`)
      and `case` with duplicate labels ("first match wins"). The uniform HIR -> MIR cascade
      described in C3 handles both: non-constant labels render as runtime `==` comparisons, and
      duplicate labels honour SV's top-down "first match wins" semantics because the cascade tests
      items in order. The same cascade also handles a corner where slang's case-context type
      unification widens the selector from a built-in form to a packed-explicit form; the snapshot
      peeks through the resulting `ConversionExpr` so the assignment matches the unwrapped source
      type. Covers archive cases `case_constant_expression`, `case_first_match_wins`. Coverage adds
      `case_constant_expression`, `case_first_match_wins`, `case_runtime_multi_label`,
      `case_runtime_no_default`.
- [x] C16 -- `case` with enum-typed selectors and enum-value labels. The uniform cascade from C3
      handles this with no new lowering: slang's case-context type unification widens the labels to
      the enum type, and the cascade's per-label `==` evaluates against the unified enum operands.
      Coverage: `case_enum_basic` (single / multi-label / default-taken) and `case_enum_no_match`
      (no default, selector unchanged from initial).
- [x] C17 -- `case` with string selector / string labels. Same cascade reused: per-label `==` uses
      the SC1 string equality helper, with the bool result projected back into the cascade's 1-bit
      truthiness shape. Coverage: `case_string_basic` (single / multi-label / default-taken) and
      `case_string_no_match`.

### Expressions

- [x] C14 -- Ternary `cond ? a : b`. New `hir::ConditionalExpr` + `mir::ConditionalExpr` carry the
      three sub-expressions; HIR -> MIR is a straight pass-through (no desugaring); the C++ backend
      renders as `(cond ? then : else)` on the native (2-state integral) path. Multi-condition
      (`&&&`) and `matches` pattern forms are rejected with `diag::Unsupported`. 4-state X/Z
      propagation on the condition follows the broader 4-state work tracked under `operators` (B7 /
      U4) and is out of scope. Coverage includes basic max-pick, nested chained, ternary embedded in
      an arithmetic expression, literal-only arms, ternary feeding an `if` condition, and ternary
      inside a `for`-loop body.

## Out of Scope

- `fork` / `join_any` / `join_none` -- scheduling primitives tracked under `processes`.
- `wait` / `wait_event` / `@(event)` -- event control, not pure control flow.
- Assertion control statements (`disable`, `restart`) -- tracked under `assertions`.
