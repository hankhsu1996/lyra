# Architecture Reset

Tracks the surfaces that existed in the pre-reset codebase and need to be supported again on the
current architecture. Each item is a chunk of work roughly the size of one to a few PRs. Pipeline
items reference layers and CLI surfaces; feature items reference the test directory under
`archived/tests/sv_features/<path>` that documents the expected behavior. When the last item lands,
this file is deleted.

An item is unchecked even when partial support already exists in the current build. Check off an
item only when the corresponding archive tests can be reproduced -- or have been intentionally
deprecated -- on the current architecture. Intentional deprecation is annotated inline with the
reason; substantive architectural decisions go in `decisions/`.

When a workstream has its own progress file (e.g. `control-flow.md`, `integral.md`), the granular
sub-step tracking lives there. The corresponding archive items below remain a high-level inventory;
the dedicated file is the source of truth for "what is done, in what order, what is blocked".

## Pipeline Surfaces

- [ ] R1 -- LIR layer implementation. Contract in `../architecture/lir.md`; no source under
      `src/lyra/lir`.
- [ ] R2 -- LLVM backend (MIR / LIR -> LLVM IR).
- [ ] R3 -- JIT execution path (ORC).
- [ ] R4 -- AOT execution path; runtime delivered as a static library.
- [ ] R5 -- CLI subcommands for executing simulations: `run`, `compile`, `dump llvm`.
- [ ] R6 -- Smoke, benchmark, and AOT CI jobs (currently `if: false`; see `../ci/README.md`).
- [x] R7 -- Test framework variable assertions (`expect.variables`). `tests/framework/runner.cpp`
      rewrites the source with a synthetic `final` probe block keyed on sentinel markers in stdout
      and matches each entry against scalar / SV-literal expectations.

## SystemVerilog Features

### assertions

- [ ] assertions/default
- [ ] assertions/deferred_assert_actions
- [ ] assertions/jit_only

### compilation_units

- [ ] compilation_units/default

### control_flow

Tracked in `control-flow.md`. Covers all `control_flow/*` archive items (case, case_inside,
conditional, do_while, for, foreach, foreach_2d, forever, repeat, ternary, unique_priority_case,
unique_priority_if, while).

### datatypes

Granular tracking lives in `integral.md` (integral / packed / wide_integral) and `datatypes.md`
(every other family). The list below remains a high-level inventory.

- [ ] datatypes/default_init
- [x] datatypes/enum
- [ ] datatypes/general
- [ ] datatypes/integral -- `integral.md`.
- [ ] datatypes/packed -- `packed.md`.
- [x] datatypes/real
- [ ] datatypes/representation
- [ ] datatypes/string
- [ ] datatypes/unpacked
- [ ] datatypes/wide_integral -- non-`packed_2d` archive sub-folders covered (`integral.md` J14);
      `packed_2d` (2D element index / slice) belongs to `datatypes/packed`.

### directives

- [ ] directives/time_literals
- [ ] directives/timescale

### dpi

- [ ] dpi/export_general
- [ ] dpi/export_pure
- [ ] dpi/import_context
- [ ] dpi/import_general
- [ ] dpi/import_pure
- [ ] dpi/svdpi_runtime
- [ ] dpi/time_query

### functions

- [ ] functions/basic
- [ ] functions/chandle
- [ ] functions/container_outparams
- [ ] functions/container_params
- [ ] functions/nested_calls
- [ ] functions/recursion
- [ ] functions/string_return

### generate

- [ ] generate/generate

### hierarchy

- [ ] hierarchy/instantiation
- [ ] hierarchy/ports
- [ ] hierarchy/refs

### lifecycle

- [ ] lifecycle/copy_assign
- [ ] lifecycle/move_aggregate

### metadata

- [ ] metadata/slot_meta

### mutation

- [ ] mutation/basic
- [ ] mutation/bulk_ops
- [ ] mutation/structural

### operators

- [x] operators/binary -- `integral.md`.
- [ ] operators/binary_string
- [x] operators/case_equality -- `operators.md`.
- [ ] operators/comparison_value_temp -- subsumed by the binary-operator coverage in `integral.md`;
      the archived `ValueTemp` IR shape does not exist on the current pipeline.
- [ ] operators/compound_assignment -- `operators.md`.
- [ ] operators/concat -- `operators.md`.
- [ ] operators/inside -- `operators.md`.
- [ ] operators/replicate -- `operators.md`.
- [ ] operators/replication_patterns -- `packed.md`.
- [x] operators/shift_overflow -- `integral.md`.
- [ ] operators/unary -- integral surface covered (`integral.md` J14); `++` / `--` archive coverage
      tracked at `integral.md` J19 (blocked on `operators.md` W12).
- [ ] operators/value_temp_expansion -- subsumed by the binary-operator coverage in `integral.md`;
      the archived `ValueTemp` IR shape does not exist on the current pipeline.
- [x] operators/wildcard_equality -- `operators.md`.

### optimization

- [ ] optimization/bounds_check_elimination
- [ ] optimization/packed_localized_write

### packages

- [ ] packages/basic
- [ ] packages/functions

### processes

Tracked in `processes.md`. Covers procedural blocks (`initial`, `final`, `always`, `always_comb`,
`always_latch`, `always_ff`), timing controls (`#N`, `@(...)`), assignments (`<=`, `assign`), and
synchronisation primitives (named events, `wait`, `fork`/`join_*`).

- [x] processes/continuous_assign -- `processes.md` P7.
- [x] processes/delay -- `processes.md` T1.
- [ ] processes/edge_trigger_bit_select -- `processes.md` T2..T5 cover constant bit-select /
      range-select / indexed part-select on packed 1D types and event lists thereof. Multi-dim
      packed, ascending / negative-base ranges, struct fields and unpacked elements are blocked on
      separate workstreams (see `processes.md` Blocked).
- [ ] processes/edge_trigger_dynamic -- blocked on compound-event snapshot + re-eval (see
      `processes.md` Blocked).
- [x] processes/event_triggers -- `processes.md` T2, T3, T4 cover the event-control surface;
      remaining named-event trigger lives in P9.
- [x] processes/final_block -- `processes.md` P2.
- [ ] processes/generate -- `processes.md` P12.
- [x] processes/initial -- `processes.md` P1.
- [ ] processes/named_event -- `processes.md` P9.
- [x] processes/non_blocking -- `processes.md` P4.
- [x] processes/wait_event -- `processes.md` P11.

### scheduling

- [ ] scheduling/edge_refresh_reconcile
- [ ] scheduling/managed_aggregate_notify
- [ ] scheduling/nba_ordering
- [ ] scheduling/nba_packed
- [ ] scheduling/nba_routing
- [ ] scheduling/nba_write_modes
- [ ] scheduling/regions
- [ ] scheduling/sub_slot_observation
- [ ] scheduling/zero_delay
- [ ] scheduling/zero_delay_time

### system_tf

- [ ] system_tf/effect
- [ ] system_tf/pure
- [ ] system_tf/state

### trace

- [ ] trace/basic
- [ ] trace/bulk_ops
- [ ] trace/signal_trace
- [ ] trace/update_set
