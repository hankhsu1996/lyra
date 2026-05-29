# Packed Types

Tracks the type-side packed-aggregate surface that is not subsumed by `operators.md`: packed
structs, packed unions, and assignment patterns over packed types. Multi-dim packed array selectors
(read and write, including chained selectors on `bit [N-1:0][W-1:0]` and deeper) are covered by
`operators.md` W4..W6 and their `vector<LvalueSelector>` chain shape; this file picks up where the
chain machinery leaves off -- field access, union views, and aggregate literal forms. Each archive
item checkbox in `architecture-reset.md` is checked when its `*.yaml` cases reproduce on the current
pipeline.

Done when:

- `datatypes/packed/packed_struct`, `packed_union`, `assignment_pattern_fill`,
  `assignment_pattern_multibit`, `nested_aggregates`, and `packed_integral_default` reproduce.
- `operators/replication_patterns` reproduces (`'{N{x}}` array-literal forms over packed types).

The numeric IDs (P3..P5) are stable references and imply execution order. The earlier P1 / P2
(multi-dim 2D / 3D selectors) are deleted from this file because they now reproduce through the
operators.md selector chain.

## Sub-Steps

- [ ] P3 -- Packed struct field access. Add a `kField(field_index)` arm to the `LvalueSelector` /
      `SelectExpr` variants alongside the existing element / range arms. HIR resolves the slang
      `MemberAccessExpression` to a `(field_index)` payload; MIR carries `(offset, width)` after
      layout. Reads and writes route through the same chain machinery W4 / W5 / W6 use, with the
      field arm contributing a constant bit offset and constant width to the accumulator. Closes
      `datatypes/packed/packed_struct`.

- [ ] P4 -- Packed union. All members overlay the same underlying `PackedArray` bits; field access
      reads / writes the full union width and reinterprets per the accessed member's type. No new
      storage primitive needed; the type system distinguishes the view. Closes
      `datatypes/packed/packed_union`.

- [ ] P5 -- Assignment patterns over packed aggregates. Handles positional `'{a, b, c}`, default
      `'{default: v}`, named `'{i: v, j: v}`, and replication-pattern `'{N{x}}` literal forms.
      Lowers to a sequence of selector-targeted writes through the W6 / P3 chain machinery; no new
      runtime primitive is needed. Closes `datatypes/packed/assignment_pattern_fill`,
      `assignment_pattern_multibit`, `nested_aggregates`, and `operators/replication_patterns`.

## Cross-references

- LRM anchors: 7.2 (packed structs and unions), 7.4 (packed and unpacked arrays), 10.9 (assignment
  patterns), 11.5.1 (bit-select / part-select on packed arrays and structs).
- Prerequisite: `operators.md` W4..W6 must land before P3 (P3 extends the same selector chain shape
  with a field arm).
- `datatypes/packed/indexed_part_select`, `datatypes/packed/packed_2d`, `packed_3d`,
  `datatypes/wide_integral/packed_2d`, and `operators/concat`, `operators/replicate`,
  `operators/compound_assignment` all live in `operators.md`, not here, even though the archive
  groups some of them under `datatypes/packed`. The split follows the type-vs-operator boundary, not
  archive directory layout.
