# Timescale and simulation time

Tracks the timescale surface: time-unit / time-precision resolution, delay scaling, the
design-global time precision, and the system functions that read and print simulation time (`$time`,
`$realtime`, `$stime`, `%t`, `$timeformat`, `$printtimescale`).

Time already advances correctly for designs that use a single time precision. This workstream closes
the two remaining gaps: the design-global precision unification that makes mixed-precision designs
correct (LRM 3.14.3), and the observation surface that lets a design read and format the current
simulation time. Done when a mixed-precision design schedules delays on one consistent time axis,
the query functions report time in the calling scope's unit, and `%t` formats time against the
`$timeformat` display unit.

The `TS*` IDs are stable references and do not impose a total order, but TS1 is the keystone the
others read from.

## Already in place

- Time unit and precision are resolved per design element from `` `timescale `` / `timeunit` /
  `timeprecision` (LRM 3.14.2, precedence per 3.14.2.3) and carried through lowering, one resolved
  unit and precision per scope.
- `#N` delays and time literals (`#5ns`) scale to integer simulation ticks. The tick is the
  design-global time precision -- the finest precision across the whole design (LRM 3.14.3) -- so a
  design that mixes precisions schedules every delay on one consistent time axis.
- The `time` and `realtime` data types exist and time literals lower to values.

## Sub-Steps

### Global precision

- [x] TS1 -- Unify on a single design-global time precision: the minimum precision across the whole
      design (LRM 3.14.3). It is the engine's tick, the denominator every delay scales against
      (numerator stays the delay's own scope unit), and the basis the query / format functions scale
      back from. Mixed-precision designs schedule on one consistent time axis; single-precision
      designs (scaling factor one) are unchanged.

### Reading simulation time

- [ ] TS2 -- `$time`, `$realtime`, `$stime` (LRM 20.3): return the current simulation time scaled to
      the time unit of the calling code's lexical scope -- the design element that contains the
      call, not the activation that runs it. Time unit is a static per-design-element property (LRM
      3.14.2), so a subroutine uses its declaration scope's unit even when enabled across a unit
      boundary (LRM 20.3.1). `$time` rounds to an integer in scope units, `$realtime` keeps the
      fractional part, `$stime` is the 32-bit truncation. Rides on TS1 for the global-precision
      basis to scale from.

### Formatting simulation time

- [ ] TS3 -- `%t` time format (LRM 21.2), `$timeformat` (LRM 20.4.3) and `$printtimescale` (LRM
      20.4.2): `%t` formats a time value against the `$timeformat` settings -- a display unit,
      fractional-digit count, suffix, and minimum field width, defaulting to the smallest precision
      across all timescale directives. The display unit is `$timeformat`'s, applied design-wide, not
      the calling scope's unit, so it is uniform across the design (distinct from TS2's per-scope
      scaling). Closes `display.md` DI2. Rides on TS1.

## Design direction

- The global time precision is a design-global value, in the same category as net resolution -- a
  design-global concern that sits outside the per-unit compilation model (`reference_resolution.md`
  carves out that category explicitly). It is not part of any unit's class-level artifact: baking it
  in would make a unit's compiled form depend on the design it happens to sit in, which
  `compilation_unit_model.md` forbids (compile-time artifacts are class-level and
  instance-independent) and which would fork the artifact per design.
- Each scope owns its own time unit and precision as class-level constants. The engine resolves the
  design-global precision at construction (time zero) as the minimum precision across the built
  scope tree -- the same construction-time resolution the architecture uses for cross-unit
  references (`reference_resolution.md`), and the runtime owns the single time axis
  (`runtime_model.md`). No precision value is threaded through the compile or emit pipeline; each
  scope reports its own and the engine takes the minimum.
- The runtime performs the two scalings from those constants and the resolved global precision: a
  delay scales from its scope's precision to the global tick, and the current time scales back from
  the global tick to a scope's unit for the query and format functions. With a single design-wide
  precision the scaling factor is one, so single-precision designs are unchanged. A synthetic node
  with no declared timescale (the implicit `$root`) does not contribute to the minimum.
- Known limitation: a `package` or compilation-unit-scope timeprecision (LRM 3.14.3) is not folded
  into the minimum, because those scopes are not instantiated into the object tree. The pre-reset
  tree had the same gap; revisit if a design relies on a package-driven precision.

## Open questions

- The exact rounding rule for a fractional delay (`#1.5`) under a coarse scope precision: round to
  the scope precision first, then scale to the global tick. Integer delays are unaffected, so TS1
  ships without it; pin the LRM rounding wording when fractional delays under a coarse precision are
  exercised.

## Out of Scope

- `step` as a time unit (LRM 3.14.3): defined as equal to the global precision but not usable to set
  or modify a unit or precision.
- SDF annotation and `$timeskew`-style back-annotation of delays.

## Cross-references

- LRM anchors: 3.14.2 (`timeunit` / `timeprecision`, precedence), 3.14.3 (global time precision),
  20.3 (simulation-time functions), 20.4 (timescale tasks), 21.2 (`%t` in the display family).
- Closes: `display.md` DI2 (`%t`).
- Archive: the pre-reset tree implemented the whole surface -- a design-global minimum-precision
  pass, the time query functions, `%t`, `$timeformat`, and `$printtimescale` -- and is a usable
  reference for all three items.
