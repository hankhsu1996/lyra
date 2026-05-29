# Datatypes

Tracks SystemVerilog data type coverage on the current pipeline. The integral family (`int`, `byte`,
`shortint`, `longint`, `time`, `integer`, `bit [N:0]`, `logic [N:0]`) lives in `integral.md`. This
file covers every other family: `datatypes/enum`, `datatypes/string`, `datatypes/unpacked`,
`datatypes/general` (dynamic array / queue / associative), `datatypes/real`,
`datatypes/default_init`, `datatypes/representation`.

Each archive item checkbox in `architecture-reset.md` is checked when its `*.yaml` cases reproduce
on the current pipeline.

## Actionable

Real C1 / C2 / C3 / C4 are complete. Structural-var declaration initializers (SV LRM 6.8) are
complete for the integral, enum, real, shortreal, and realtime families (SI1 below). Remaining
families: `datatypes/string`, `datatypes/unpacked`, `datatypes/general`, `datatypes/default_init`,
`datatypes/representation`.

## Enum

Covers archive item `datatypes/enum` (sub-folders `enum`, `enum_implicit_conversion`,
`enum_methods`). LRM 6.19 semantics: enum types are a labelled subset of an integral base type; enum
-> integral conversion is implicit, integral -> enum requires an explicit cast.

- [x] E1 -- Declarations, member references, comparisons, arithmetic, and conversions. Covers
      `enum/default.yaml` (basic, sequential, explicit, mixed, comparison, arithmetic, explicit base
      type, display, range count / bounds / mixed); legal cases in
      `enum_implicit_conversion/default.yaml`; and the `first` / `last` / `num` / `num_auto_values`
      cases in `enum_methods/default.yaml`.
- [x] E2 -- `next` / `prev` methods with optional step argument (LRM 6.19.5.3 / 6.19.5.4). Covers
      the `enum_next*`, `enum_prev*`, and `enum_next_runtime_step` cases in
      `enum_methods/default.yaml`. Non-member receivers fall back to a zero default; LRM Table 6-7
      4-state `'x` behaviour is tracked under `datatypes/default_init`.
- [x] E3 -- `name()` method (LRM 6.19.5.6); empty string for non-member values. Covers `enum_name`
      in `enum_methods/default.yaml`.

### Cross-references

- LRM 6.19 (Enumerations), 6.19.3 (conversions), 6.19.5 (methods).
- Archive items: `datatypes/enum/{enum,enum_implicit_conversion,enum_methods}`.
- Unblocks: C16 (enum-typed `case` selectors) in `control-flow.md`.

## Real

Covers archive item `datatypes/real` (sub-folders `real_types`, `shortreal_types`,
`realtime_types`). Per LRM 6.12, `real` is IEEE 754 double, `shortreal` is IEEE 754 single, and
`realtime` is synonymous with `real`. LRM 6.12 forbids edge event controls on real variables.

- [x] C1 -- Declarations, blocking assignment, and `$display` formatting (`%f` / `%e` / `%g` with
      LRM Table 21-2 default precision). Covers the declaration / assignment / display cases across
      all three archive sub-folders.
- [x] C2 -- Operators legal on real per LRM 11.3.1 (arithmetic, relational, equality, logical,
      conditional), plus `**` via the LRM 11.4.3 result-typing rule, plus shortreal <-> real
      conversion.
- [x] C3 -- Cross-family conversions: integral -> real treats `'x` / `'z` bits as 0 per LRM 6.12.1;
      real -> integral rounds half-away-from-zero per LRM 6.12.1 (not truncate).
- [x] C4 -- LRM-illegal real forms (LRM 6.12 + 11.3.1 / Table 11-1) -- edge event controls,
      bit-select / part-select, real-indexed bit-select, modulus, bitwise, reduction, shift,
      wildcard equality, and case equality on real -- diagnose as `diag::Unsupported` with an LRM
      citation. The slang frontend filters all but case equality (`===` / `!==`) at AST
      construction; the case-equality path is guarded by `errors/real_case_equality_unsupported`.

### Cross-references

- LRM 6.12 (Real, shortreal, realtime), 6.12.1 (Conversion), 11.3.1 (Operators with real operands),
  Table 11-1 (Operators and data types).
- Archive items: `datatypes/real/{real_types,shortreal_types,realtime_types}`.

## Structural Initializers

Covers SV LRM 6.8: a static-lifetime variable declaration may carry an initializer expression
(`int i = 42;`, `real r = 1.5;`, `logic [7:0] l = 8'hAA;`). The LRM requires the initial-value
assignment to run before any `initial` / `always` procedure starts, which the emitted code satisfies
by lowering the initializer onto the C++ field declaration.

- [x] SI1 -- Declaration initializers for the integral, enum, real, shortreal, and realtime
      families. Covers `real_declaration_with_init` (real) plus new cases `int_structural_init`,
      `bit_structural_init`, `byte_structural_init`, `longint_structural_init`,
      `logic_structural_init`.
- [ ] SI2 -- Initializer expressions that read a module parameter (`int i = N * 3;`) or contain a
      string literal (`string s = "hello";`). Both are surfaced as `diag::Unsupported` today -- the
      gap is in structural-expression lowering, not in the initializer mechanism, but it blocks the
      natural parameter-driven init pattern.

### Cross-references

- LRM 6.8 (Variable declarations -- "Setting the initial value of a static variable... shall occur
  before any initial or always procedures are started"); Table 6-7 (Default initial values, used
  when no initializer is present, tracked separately under `datatypes/default_init`).
