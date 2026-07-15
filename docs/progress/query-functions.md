# Query functions

Tracks support for the SystemVerilog data-query and array-query system functions (LRM 20.6, 20.7):
`$bits`, `$typename`, the array dimension queries (`$left`, `$right`, `$low`, `$high`, `$increment`,
`$size`), and the dimension-count queries (`$dimensions`, `$unpacked_dimensions`). These functions
report properties of a data type or of an array's shape, not a computation over a value.

Done when the whole 20.6 / 20.7 family lowers and runs for every data type: the fixed-size forms as
elaboration-time constants, and the dynamically sized forms as runtime queries of the array's
current state.

## Semantic model

The split is not between queries but between the facts a query reports. Whatever the operand's type
fixes is an **elaboration-time constant**; only what the value's current state decides is read at
run time.

- The operand is never evaluated (LRM 20.6.2: `$bits` is "determined without actual evaluation of
  the expression"; 20.6.1: `$typename`'s expression "is never evaluated"), and it may be a data type
  rather than a value, which has no runtime form at all. A type cannot depend on a genvar, so every
  type-fixed result is a literal at the query site. This is distinct from the select-bound rule (a
  bound may be a genvar-dependent runtime value and is lowered faithfully): a query operand has no
  runtime form to be faithful to.
- A dynamically sized dimension (dynamic array, queue, associative array, string) reports its
  **current state** (LRM 20.7: "When used on a dynamic array or queue dimension, these functions
  return information about the current state"; 20.6.2: `$bits` of a dynamically sized value returns
  its current bit count, 0 when empty). Only the dimension's extent is such a fact. Its direction
  follows from its kind -- a queue or dynamic array dimension ascends from 0 -- and an associative
  dimension's index space follows from its index type, so those remain constants even though the
  dimension is dynamic. It is an error to apply these directly to a dynamically sized type
  identifier, which the frontend rejects.
- The dimension argument need not be a constant expression, and neither the LRM nor the frontend
  requires one. An index the simulation supplies selects among the per-dimension results, which the
  operand's type still fixes.

`$dimensions` and `$unpacked_dimensions` count dimensions, a type property, so they are constant for
every operand, dynamic included.

## Sub-steps

- [x] Q1 -- `$bits` of a type or a fixed-size value, as an elaboration-time constant (LRM 20.6.2).
      The operand is not evaluated; the result is the bit count of the operand's type, usable
      wherever an integer constant is, including as a part-select bound. Unblocks the `ibex_top`
      reduction over a `$bits`-derived part-select tracked in `ibex.md`.
- [x] Q2 -- The array query functions on a fixed-size dimension, as elaboration-time constants (LRM
      20.7): `$left`, `$right`, `$low`, `$high`, `$increment`, `$size`, with the optional constant
      dimension argument, plus `$dimensions` and `$unpacked_dimensions` for every type. Covers
      packed and unpacked, single- and multi-dimensional shapes.
- [x] Q3 -- `$typename` as an elaboration-time string constant (LRM 20.6.1), for a type or an
      expression operand.
- [x] Q4 -- The runtime forms over a dynamically sized operand: `$bits` of a dynamically sized value
      (0 when empty), and the array queries on a dynamic / queue / associative dimension reporting
      current state (LRM 20.6.2, 20.7). Only a dimension's extent depends on the current state; its
      direction follows from its kind, and an associative dimension's index space from its index
      type, so those stay elaboration-time constants. An associative dimension reports its index
      type's default while no index is allocated, which is the `'x` LRM 20.7 requires when that type
      is 4-state.
- [x] Q5 -- A dimension named by an expression the running simulation supplies (LRM 20.7 does not
      require the dimension to be a constant expression). The query selects among the per-dimension
      results, which the operand's type still fixes; an index the type has no dimension for reads
      `'x`.
- [ ] Q6 -- A dimension named at run time over an array that has a dynamically sized dimension. LRM
      20.7.1 makes it an error for such an index to name that dimension; an index only the
      simulation knows cannot be held to that rule at elaboration, so the form is rejected rather
      than half-served. Reporting the error when the index actually lands on that dimension is what
      the form needs.
- [ ] Q7 -- `$bits` of a value whose elements are themselves dynamically sized (a dynamic array of
      dynamic arrays, a queue of strings). Every other dynamically sized operand reports its element
      count times the bits one element occupies; a dynamically sized element has no such fixed
      width, so the count has to be summed over the elements.

## LRM ambiguity

LRM 20.7 lists the dimensions these functions accept as fixed-size, dynamic, associative, or queue,
and does not say which of those a `string` is, while LRM 20.7's `$dimensions` counts a string as one
dimension. A string dimension is treated here as its characters: `$size` is the character count and
the dimension ascends from 0, which agrees with the frontend's own typing and with `$bits` of the
same string.

## Out of scope

- `$isunbounded` (LRM 20.6.3) queries whether a parameter is the unbounded `$`; it belongs with the
  unbounded-parameter work noted out of scope in `hierarchy.md`, not with the shape queries here.
- The value-computation system functions -- `$clog2` (LRM 20.8), `$signed` / `$unsigned` (LRM 20.5),
  `$isunknown` / `$countones` / `$onehot` (LRM 20.9) -- compute over a value rather than query a
  type or shape. They are a separate family and are already supported.

## Cross-references

- `ibex.md` -- Q1 unblocks the `ibex_top` structural reduction gap.
