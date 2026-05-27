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
- [ ] R7 -- Test framework variable assertions (`expect.variables`). Archived tests assert
      end-of-simulation values directly (`c: 30`, `r: "4'bx01x"`); the current framework supports
      only `exit`/`stdout`/`stderr`, forcing every behavioral test to route through `$display`.
      Blocks faithful reproduction of most feature tests under `archived/tests/sv_features/`.

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
- [ ] datatypes/enum
- [ ] datatypes/general
- [ ] datatypes/integral -- `integral.md`.
- [ ] datatypes/packed -- `packed.md`.
- [ ] datatypes/real
- [ ] datatypes/representation
- [ ] datatypes/string
- [ ] datatypes/unpacked
- [ ] datatypes/wide_integral -- `integral.md`.

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

- [ ] operators/binary -- `integral.md`.
- [ ] operators/binary_string
- [ ] operators/case_equality -- `operators.md`.
- [ ] operators/comparison_value_temp -- subsumed by the binary-operator coverage in `integral.md`;
      the archived `ValueTemp` IR shape does not exist on the current pipeline.
- [ ] operators/compound_assignment -- `operators.md`.
- [ ] operators/concat -- `operators.md`.
- [ ] operators/inside -- `operators.md`.
- [ ] operators/replicate -- `operators.md`.
- [ ] operators/replication_patterns -- `packed.md`.
- [ ] operators/shift_overflow -- `integral.md`.
- [ ] operators/unary -- `integral.md` for the integral surface; `++` / `--` gap in `operators.md`.
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

- [ ] processes/continuous_assign
- [ ] processes/delay
- [ ] processes/edge_trigger_bit_select
- [ ] processes/edge_trigger_dynamic
- [ ] processes/event_triggers
- [ ] processes/final_block
- [ ] processes/generate
- [ ] processes/initial
- [ ] processes/named_event
- [ ] processes/non_blocking
- [ ] processes/wait_event

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
