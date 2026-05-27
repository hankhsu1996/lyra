# Packed Types

Tracks the type-side packed-aggregate surface: multi-dimensional packed arrays, packed structs,
packed unions, and assignment patterns for packed types. The operator-side selector machinery
(single-layer bit / range / indexed part-select read and write) lives in `operators.md` and is a
prerequisite for everything here. Each archive item checkbox in `architecture-reset.md` is checked
when its `*.yaml` cases reproduce on the current pipeline.

Done when:

- `datatypes/packed/packed_2d`, `packed_3d`, `packed_struct`, `packed_union`,
  `assignment_pattern_fill`, `assignment_pattern_multibit`, `nested_aggregates`, and
  `packed_integral_default` reproduce.
- `datatypes/wide_integral/packed_2d` reproduces (a 2D-indexing case wrongly grouped under
  `wide_integral` in the archive; it belongs to this workstream).
- `operators/replication_patterns` reproduces (`'{N{x}}` array-literal forms over packed types).

The numeric IDs (P1..P5) are stable references and imply execution order.

## Sub-Steps

- [ ] P1 -- Multi-dim packed 2D element select. Type `bit [N-1:0][W-1:0] x` exposes `x[i]` as a
      W-bit slice on both read and write. Extend the HIR / MIR `Lvalue` selector list from the
      single-layer `optional<Selector>` introduced in `operators.md` W7 to `vector<Selector>`, and
      extend `SelectExpr` analogously on the read side. `PackedArray` itself keeps a flat layout;
      the type system carries the dimension stack so each selector picks the right contiguous bit
      range. Closes `datatypes/packed/packed_2d` and `datatypes/wide_integral/packed_2d`.

- [ ] P2 -- Multi-dim packed 3D. Same selector list machinery, exercised at depth three. Adds
      out-of-range and X / Z index tests at every level. Closes `datatypes/packed/packed_3d`.

- [ ] P3 -- Packed struct field access. Add a `kField(field_index)` arm to the `Selector` variant
      (in addition to W4's bit / range / indexed arms). HIR resolves the slang
      `MemberAccessExpression` to a `(field_index)` payload; MIR carries `(offset, width)` after
      layout. Reads and writes route through the same `PackedArray::ReadRange` / `WriteRange`
      helpers as W5 / W7. Closes `datatypes/packed/packed_struct`.

- [ ] P4 -- Packed union. All members overlay the same underlying `PackedArray` bits; field access
      reads / writes the full union width and reinterprets per the accessed member's type. No new
      storage primitive needed; the type system distinguishes the view. Closes
      `datatypes/packed/packed_union`.

- [ ] P5 -- Assignment patterns over packed aggregates. Handles positional `'{a, b, c}`, default
      `'{default: v}`, named `'{i: v, j: v}`, and replication-pattern `'{N{x}}` literal forms.
      Lowers to a sequence of selector-targeted writes through the W7 / P1 machinery; no new runtime
      primitive is needed. Closes `datatypes/packed/assignment_pattern_fill`,
      `assignment_pattern_multibit`, `nested_aggregates`, and `operators/replication_patterns`.

## Cross-references

- LRM anchors: 7.2 (packed structs and unions), 7.4 (packed and unpacked arrays), 10.9 (assignment
  patterns), 11.5.1 (bit-select / part-select on packed arrays and structs).
- Prerequisite: `operators.md` W4..W7 must land before P1. P3 additionally needs the `Selector`
  variant to be open for extension; introduce the `kField` arm in P3 itself.
- `datatypes/packed/indexed_part_select` and `operators/concat`, `operators/replicate`,
  `operators/compound_assignment` all live in `operators.md`, not here, even though the archive
  groups some of them under `datatypes/packed`. The split follows the type-vs-operator boundary, not
  archive directory layout.
