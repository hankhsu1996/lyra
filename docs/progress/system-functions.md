# System functions

Tracks the LRM Clause 20 utility system tasks and functions that remain unimplemented and have no
home in another progress file. The computational families run today -- array and data query (LRM
20.6, 20.7), integer and real math (LRM 20.8), the bit-vector functions (LRM 20.9), and the real and
sign conversions (LRM 20.5); `tests/conformance/` is the record of which. What is left is the
control and query leftovers queued below, plus the domain families each tracked with its own
feature.

Done when the queued items land and every remaining system-function gap either runs or is owned by
the feature file its domain belongs to.

## Queued

- [x] Simulation control beyond `$finish` (LRM 20.2): `$stop`, which suspends the run, and `$exit`,
      which ends it once every program block has finished. The diagnostic-level argument (0 / 1 / 2)
      and its end-of-simulation reporting are the axis `$finish` shares, tracked as `processes.md`
      P14; this item is the two entry points and their termination behaviour.
- [ ] `$isunbounded` (LRM 20.6). Answers whether a parameter's value is the unbounded `$`; a data
      query beside `$bits` and `$typename`, which run.

## Tracked with their domain

- The output family's continuous monitor -- `$monitor`, `$fmonitor`, `$monitoron`, `$monitoroff`
  (LRM 21.2.3) -- in `display.md`.
- Sampled value functions -- `$sampled`, `$past`, `$rose`, `$fell`, `$stable`, `$changed` (LRM
  16.9.3) -- and the assertion control tasks (LRM 20.12), in `assertions.md` (AS4, AS6).
- `$cast` (LRM 6.24.2), the checked run-time conversion, is class-object-model work rather than a
  utility function, so it belongs with that family and not here.

## Out of scope

No concrete driver; listed so an encounter is read as a known gap rather than a compiler bug:

- Functional coverage system functions (LRM 20.13).
- VCD value-change dump -- `$dumpfile`, `$dumpvars`, `$dumpon`, `$dumpoff`, `$dumpall`,
  `$dumplimit`, `$dumpflush`, and the `$dumpports` variants (LRM 21.7).
- `$sdf_annotate` (LRM 20.14), `$countdrivers`, `$getpattern`, `$sreadmemb` / `$sreadmemh`, the
  interactive `$list` / `$scope` / `$input`, and the PLA modeling tasks (LRM 20.16).
- `$psprintf`, a vendor alias for `$sformatf`, which runs.
