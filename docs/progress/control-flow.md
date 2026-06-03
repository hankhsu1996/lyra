# Control Flow

Tracks procedural control-flow constructs. Covers archive items under
`archived/tests/sv_features/control_flow/`.

The numeric IDs (C1..C17) are stable references to archive items and do **not** imply execution
order.

## Actionable

C9 is done. C10's skipped-dimension and trailing-dim subset rides on C9 because the lowering shape
is uniform across dim counts; the dynamic-array, queue, and associative-array subset stays open and
remains blocked on `datatypes/general`.

| Item | Status                                                              |
| ---- | ------------------------------------------------------------------- |
| C10  | Blocked on `datatypes/general` (dynamic array, queue, associative). |

## Sub-Steps

### Statements

- [x] C1 -- `if` / `else` (procedural conditional). Multi-condition lists, pattern forms, and
      `unique` / `priority` qualifiers are rejected as unsupported; the qualifier work is C13.
- [x] C2 -- `for` (procedural for-loop). Includes inline and external init, multi-init / multi-step,
      countdown, zero-iteration, and `if`-in-body composition. `break` / `continue` are C7.
- [x] C3 -- `case` (plain, `==` semantics, LRM 12.5). Reproduces the core archive subset: basic
      match, default, multi-label, no-default-no-match, empty body, nested. Selector "evaluated
      once" semantics are preserved through a selector snapshot. Enum / string selectors are tracked
      as C16 / C17.
- [x] C4 -- `while`. Includes runtime conditions, countdown, `while (0)` zero iterations, two-level
      nesting, and mixed nesting with `for`. `break` is C7.
- [x] C5 -- `repeat (N)` with LRM "evaluate count once" semantics. Includes literal and runtime
      counts, zero iterations, nesting, single-statement body, and mixed nesting.
- [x] C6 -- `do_while`. Includes basic loops, `do {...} while (0)` (body still executes once),
      runtime conditions, nesting, single-statement body, and mixed nesting.
- [x] C7 -- Loop control: `break`, `continue`. Honours the LRM rule of exiting only the innermost
      surrounding loop, including the `repeat (N)` case where break / continue land inside the
      desugared inner loop. Coroutine interaction (early-exit across timing inside loop bodies) is
      not yet exercised because timed loop bodies are not supported.
- [x] C8 -- `forever`. Loops until `break` or `$finish` exits. Includes `$finish` termination,
      `break` termination, `continue`, single-statement body, nested forever with inner `break`, and
      `if`-guarded forever.
- [x] C9 -- `foreach` over fixed packed and unpacked arrays of any dimensionality. Single lowering
      handles 1D, 2D, N-D, mixed packed/unpacked nests, ascending and descending ranges, non-zero
      and negative bases. The lowered HIR is a single flattened `ForStmt` whose body computes per-
      iteration loop-variable values from a synthetic flat counter, so plain SV `break` / `continue`
      / `return` map 1:1 to LRM 12.8 semantics without any rewrite. See
      `../decisions/foreach-lowering.md`.
- [ ] C10 -- `foreach` skipped dimensions, dynamic-array, queue. Skipped dims and trailing dim
      omission (`int arr[2][3]; foreach (arr[i])`) ship with C9 because they fall out of the same
      uniform lowering. Dynamic-array, queue, and associative-array foreach are rejected with
      `kUnsupportedStatementForm` until `datatypes/general` brings procedural support for those
      types and the `.size()` runtime query.
- [x] C11 -- `casez` / `casex` (LRM 12.5.1 do-not-care forms). Bidirectional wildcard compare:
      `casez` treats Z as don't-care on either operand; `casex` treats Z or X as don't-care on
      either operand. Result is deterministic, distinct from the asymmetric `==?` operator (LRM
      11.4.6) which can yield `1'bx`.
- [x] C12 -- `case (... inside ...)` (LRM 12.5.4). Selector snapshot plus per-item inside-membership
      with `==?` for value items and `(>= lo) && (<= hi)` for ranges; OR-reduction across items.
      `1'bx` from inside is correctly excluded from match per LRM 12.5.4 ("match only when inside
      returns `1'b1`").
- [x] C13 -- `unique` / `unique0` / `priority` qualifiers on `if` and `case`. Each branch's
      predicate is snapshotted; the qualifier check runs in the Observed region and emits a runtime
      warning when violated (`unique`: count != 1; `unique0`: count > 1; `priority`: count == 0).
      Glitch-driven false positives within one time slot are suppressed. Source-location reporting
      in the warning text is a follow-up.
- [x] C15 -- `case` with non-constant labels and with duplicate labels (first-match-wins per LRM
      12.5). The C3 cascade handles both: non-constant labels render as runtime `==`; duplicate
      labels honour top-down ordering.
- [x] C16 -- `case` with enum-typed selectors and enum-value labels (LRM 12.5 with LRM 6.19 type
      unification).
- [x] C17 -- `case` with string selector / string labels. Per-label compare uses the SC1 string
      equality helper from `datatypes.md`.

### Expressions

- [x] C14 -- Ternary `cond ? a : b`. Multi-condition (`&&&`) and `matches` pattern forms are
      rejected as unsupported. 4-state X/Z propagation on the condition rides on the broader 4-state
      work in `operators.md`.

## Out of Scope

- `fork` / `join_any` / `join_none` -- tracked under `processes.md`.
- `wait` / `wait_event` / `@(event)` -- event control, tracked under `processes.md`.
- Assertion control statements (`disable`, `restart`) -- tracked under `assertions`.
