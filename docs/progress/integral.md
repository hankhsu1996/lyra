# Integral Representation

Tracks SystemVerilog integral coverage on the current pipeline: `byte`, `shortint`, `int`,
`longint`, `integer`, `time`, `bit [N:0]`, `logic [N:0]`, `reg [N:0]`, and wide (>64-bit) packed
variants.

Done when every archive case under `operators/binary`, `operators/unary`,
`operators/shift_overflow`, `datatypes/integral`, and `datatypes/wide_integral` reproduces.

## Actionable

All items closed; integral surface complete.

## Sub-Steps

The numeric IDs are stable references and do not imply execution order.

- [x] J1 -- Declarations, assignment, and `$display` round-trip for every integral type, including
      X/Z preservation on `integer` and `time` (LRM 6.11 Table 6-8).
- [x] J6 -- Bitwise operators (`&`, `|`, `^`, `~^`, `~`) with 4-state truth tables.
- [x] J7 -- Arithmetic operators (`+`, `-`, `*`, `/`, `%`, `**`) with LRM 11.4 corner cases
      (div-by-zero, power semantics).
- [x] J8 -- Comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`) returning a 1-bit result.
- [x] J9 -- Shift operators (`<<`, `>>`, `>>>`) with LRM 11.4.10 amount-overflow semantics
      (`amount >= bit_width` yields 0 or sign-fill).
- [x] J10 -- Logical operators (`&&`, `||`, `!`, `->`, `<->`).
- [x] J11 -- Reduction operators (`&`, `|`, `^`, `~&`, `~|`, `~^`).
- [x] J12 -- 4-state X/Z propagation across every operator family above (LRM 11.8.4 truth tables and
      X-poisoning rules).
- [x] J13 -- Wide (>64-bit) coverage on arithmetic, comparison, shift, divmod, and power, with
      4-state propagation intact across word boundaries.
- [x] J14 -- Archive sweep on the integral operator surface. Closes the archive contract for
      everything in scope: signed boundary, mixed-width promotion (LRM 11.6 / 11.7), cross-word
      bitwise / reduction, and narrow <-> wide zero/sign-extend + truncation. `++` / `--` is split
      out as J19 because it is read-modify-write rather than a pure operator and rides on the lvalue
      surface.
- [x] J17 -- Case equality `===` / `!==` (LRM 11.4.5): bit-pattern compare, deterministic bool; also
      drives event-control change detection (LRM 9.4.2 "not equal to its previous value").
- [x] J18 -- Wildcard equality `==?` / `!=?` (LRM 11.4.6): RHS X/Z treated as don't-care. Same work
      item as `operators.md` W2 (canonical home).
- [x] J19 -- `++` / `--` archive coverage (the five increment / decrement cases in
      `operators/unary/two_state.yaml`). Shipped together with `operators.md` W12.

## Cross-references

- LRM anchors: 6.11 (Table 6-8 integer types), 6.11.2 (4-state to 2-state convert), 10.7 (assignment
  extension / truncation), 11.4.5 (case equality), 11.4.6 (wildcard equality), 11.4.10 (shift), 11.6
  (expression bit lengths), 11.7 (signed expressions), 11.8 (expression evaluation rules, 11.8.4 for
  X/Z handling).
- Archive items targeted: `operators/binary`, `operators/unary` (excluding `++` / `--`, J19),
  `operators/shift_overflow`, `datatypes/integral`, `datatypes/wide_integral`. The `packed_2d`
  sub-folder is part of `datatypes/packed`, not this workstream.
- Decision and findings: `../decisions/integral-representation.md`.
