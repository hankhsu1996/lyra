# Architecture Reset

Tracks the surfaces that existed in the pre-reset codebase and need to be supported again on the
current architecture. Each item is a chunk of work roughly the size of one to a few PRs. Pipeline
items reference layers and CLI surfaces; feature items reference the test directory under
`archived/tests/sv_features/<path>` that documents the expected behavior. When the last item lands,
this file is deleted.

An item is unchecked even when partial support already exists in the current build. Check off an
item only when the corresponding archive tests can be reproduced -- or have been intentionally
deprecated -- on the current architecture. "Intentionally deprecated" requires a `decisions/` entry;
absent that, the item stays open.

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

- [ ] control_flow/case
- [ ] control_flow/case_inside
- [ ] control_flow/conditional
- [ ] control_flow/do_while
- [ ] control_flow/for
- [ ] control_flow/foreach
- [ ] control_flow/foreach_2d
- [ ] control_flow/forever
- [ ] control_flow/repeat
- [ ] control_flow/ternary
- [ ] control_flow/unique_priority_case
- [ ] control_flow/unique_priority_if
- [ ] control_flow/while

### datatypes

- [ ] datatypes/default_init
- [ ] datatypes/enum
- [ ] datatypes/general
- [ ] datatypes/integral
- [ ] datatypes/packed
- [ ] datatypes/real
- [ ] datatypes/representation
- [ ] datatypes/string
- [ ] datatypes/unpacked
- [ ] datatypes/wide_integral

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

- [ ] operators/binary
- [ ] operators/binary_string
- [ ] operators/case_equality
- [ ] operators/comparison_value_temp
- [ ] operators/compound_assignment
- [ ] operators/concat
- [ ] operators/inside
- [ ] operators/replicate
- [ ] operators/replication_patterns
- [ ] operators/shift_overflow
- [ ] operators/unary
- [ ] operators/value_temp_expansion
- [ ] operators/wildcard_equality

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
