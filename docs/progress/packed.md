# Packed Types

Tracks the type-side packed-aggregate surface: packed structs, packed unions, and assignment
patterns over packed types. Multi-dim packed array selectors (read and write) live in `operators.md`
W4..W6 and reuse the selector chain shape there; this file picks up where the chain machinery leaves
off -- field access, union views, and aggregate literal forms.

Done when:

- `datatypes/packed/packed_struct`, `packed_union`, `assignment_pattern_fill`,
  `assignment_pattern_multibit`, `nested_aggregates`, and `packed_integral_default` reproduce.
- `operators/replication_patterns` reproduces (`'{N{x}}` array-literal forms over packed types).

## Actionable

P3, P4, P5 are all open and ordered: P3 introduces the field selector and unblocks P4 (union field
views ride on the same shape); P5 builds assignment patterns on top.

| Item | Status                                                    |
| ---- | --------------------------------------------------------- |
| P3   | Ready when `operators.md` W4..W6 are in place (they are). |
| P4   | Depends on P3.                                            |
| P5   | Depends on P3.                                            |

## Sub-Steps

The numeric IDs are stable references.

- [ ] P3 -- Packed struct field access (LRM 7.2.1). Field selector arm on the selector chain,
      contributing constant bit offset and constant width. Closes `datatypes/packed/packed_struct`.

- [ ] P4 -- Packed union (LRM 7.3.1). All members overlay the same underlying bits; field access
      reads / writes the full union width and reinterprets per the accessed member's type. The type
      system distinguishes the view. Closes `datatypes/packed/packed_union`.

- [ ] P5 -- Assignment patterns over packed aggregates (LRM 10.9): positional `'{a, b, c}`, default
      `'{default: v}`, named `'{i: v, j: v}`, and replication `'{N{x}}`. Lowers to a sequence of
      selector-targeted writes through the W6 / P3 chain. Closes
      `datatypes/packed/assignment_pattern_fill`, `assignment_pattern_multibit`,
      `nested_aggregates`, and `operators/replication_patterns`.

## Cross-references

- LRM anchors: 7.2 (packed structs and unions), 7.4 (packed and unpacked arrays), 10.9 (assignment
  patterns), 11.5.1 (bit-select / part-select on packed arrays and structs).
- Prerequisite: `operators.md` W4..W6 (chain shape this file extends).
- `datatypes/packed/indexed_part_select`, `datatypes/packed/packed_2d`, `packed_3d`,
  `datatypes/wide_integral/packed_2d`, and `operators/{concat,replicate,compound_assignment}` are
  tracked under `operators.md`, not here, even though the archive groups some under
  `datatypes/packed`. The split follows the type-vs-operator boundary, not archive layout.
