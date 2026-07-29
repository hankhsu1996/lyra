# Packed Types

Tracks the type-side packed-aggregate surface: packed structs, packed unions, and assignment
patterns over packed types. Multi-dim packed array selectors (read and write) live in `operators.md`
W4..W6; this file picks up where they leave off -- field access, union views, and aggregate literal
forms.

Done when:

- `datatypes/packed/packed_struct`, `packed_union`, `assignment_pattern_fill`,
  `assignment_pattern_multibit`, `nested_aggregates`, and `packed_integral_default` reproduce.
- `operators/replication_patterns` reproduces (`'{N{x}}` array-literal forms over packed types).

## Actionable

The sub-steps are complete; the one open increment is the assignment pattern as an assignment target
(the checkbox under P5).

## Sub-Steps

The numeric IDs are stable references.

- [x] P3 -- Packed struct field access (LRM 7.2.1). Reads and writes through `s.f`, composing with
      element / range selectors on either side (`s[0]`, `s[3:0]`, `s.f[3:0]`). Whole-struct copy,
      equality, and arithmetic ride on the "treated as a single vector" projection. Mixed 2-state /
      4-state member conversion (LRM 7.2.1 fourth paragraph) tracks the struct-level atom; per-field
      state-promotion at the boundary is a follow-up.

- [x] P4 -- Packed union (LRM 7.3.1 / 7.3.2), tagged and untagged. Untagged hard and soft unions
      overlay their members at the LSBs, and reads / writes reinterpret per the accessed member's
      declared type, including signed reinterpretation. A tagged union additionally carries a tag at
      the MSBs naming the member held: construction sets tag and member together, the bits between
      them are undefined, and a dot-notation read or write that disagrees with the tag is a run-time
      error (LRM 11.9), including when the tag itself is unknown. The tag width is what separates
      the two forms, so an untagged union is the case where nothing distinguishes the members.
      Whole-union copy, equality, bit / part selects, `$bits`, and default initialization all ride
      on the "treated as a single vector" projection. Pattern matching over a tagged packed union
      (LRM 12.6) is covered in `control-flow.md`.

- [x] P5 -- Assignment patterns over packed aggregates (LRM 10.9). Positional `'{a, b, c}`, named /
      type-key / index-key `'{x: v, default: w}`, `'{default: v}`, and replication `'{N{items}}`
      over packed structs and packed arrays. The four non-replicated forms collapse into a per-field
      expression list at the slang binding boundary; replication preserves the LRM shape and unrolls
      into the same packed concat path used for `{a,b,c}`. The type-prefixed self-determined form
      `T'{...}` is in. Closes `datatypes/packed/assignment_pattern_fill` and
      `datatypes/packed/assignment_pattern_multibit`. Unpacked-array targets are deferred behind the
      unpacked-array push.
  - [ ] The assignment pattern as an assignment target (LRM 10.9): the LHS-destructuring form
        `'{a, b, c} = B` is rejected with a diagnostic. The concatenation spelling `{a, b, c} = B`
        (LRM 11.4.12) is supported, so only the pattern spelling is missing.

## Cross-references

- LRM anchors: 7.2 (packed structs), 7.3 (packed unions), 7.4 (packed and unpacked arrays), 10.9
  (assignment patterns), 11.5.1 (bit-select / part-select on packed arrays, structs, and unions).
- Prerequisite: `operators.md` W4..W6 (P3 adds packed-struct field access as a new addressable
  expression form on top of the existing selectors).
- `datatypes/packed/indexed_part_select`, `datatypes/packed/packed_2d`, `packed_3d`,
  `datatypes/wide_integral/packed_2d`, and `operators/{concat,replicate,compound_assignment}` are
  tracked under `operators.md`, not here, even though the archive groups some under
  `datatypes/packed`. The split follows the type-vs-operator boundary, not archive layout.
