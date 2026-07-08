# Query functions

Tracks support for the SystemVerilog data-query and array-query system functions (LRM 20.6, 20.7):
`$bits`, `$typename`, the array dimension queries (`$left`, `$right`, `$low`, `$high`, `$increment`,
`$size`), and the dimension-count queries (`$dimensions`, `$unpacked_dimensions`). These functions
report properties of a data type or of an array's shape, not a computation over a value.

Done when the whole 20.6 / 20.7 family lowers and runs for every data type: the fixed-size forms as
elaboration-time constants, and the dynamically sized forms as runtime queries of the array's
current state. When the last item lands, this file is deleted.

## Semantic model

These functions split on one axis, fixed by the LRM:

- A query whose operand is a type, or a value of fixed-size type, is an **elaboration-time
  constant**. The operand is never evaluated (LRM 20.6.2: `$bits` is "determined without actual
  evaluation of the expression"; 20.6.1: `$typename`'s expression "is never evaluated"). The result
  is a property of the type, fixed per specialization -- a type cannot depend on a genvar, and any
  dimension argument is itself a constant expression -- so it is a literal at the query site, not a
  runtime read. This is distinct from the select-bound rule (a bound may be a genvar-dependent
  runtime value and is lowered faithfully): a query operand has no runtime form to be faithful to.
- A query whose operand is a dynamically sized value (dynamic array, queue, associative array,
  string) reports the array's **current state** and is a genuine runtime query (LRM 20.7: "When used
  on a dynamic array or queue dimension, these functions return information about the current
  state"; 20.6.2: `$bits` of a dynamically sized value returns its current bit count, 0 when empty).
  It is an error to apply these directly to a dynamically sized type identifier.

`$dimensions` and `$unpacked_dimensions` count dimensions, a type property, so they are constant for
every operand, dynamic included.

## Sub-steps

- [x] Q1 -- `$bits` of a type or a fixed-size value, as an elaboration-time constant (LRM 20.6.2).
      The operand is not evaluated; the result is the bit count of the operand's type, usable
      wherever an integer constant is, including as a part-select bound. Unblocks the `ibex_top`
      reduction over a `$bits`-derived part-select tracked in `ibex.md`.
- [ ] Q2 -- The array query functions on a fixed-size dimension, as elaboration-time constants (LRM
      20.7): `$left`, `$right`, `$low`, `$high`, `$increment`, `$size`, with the optional constant
      dimension argument, plus `$dimensions` and `$unpacked_dimensions` for every type. Covers
      packed and unpacked, single- and multi-dimensional shapes.
- [ ] Q3 -- `$typename` as an elaboration-time string constant (LRM 20.6.1), for a type or an
      expression operand.
- [ ] Q4 -- The runtime forms over a dynamically sized operand: `$bits` of a dynamically sized value
      (0 when empty), and the array queries on a dynamic / queue / associative dimension reporting
      current state (LRM 20.6.2, 20.7).

## Out of scope

- `$isunbounded` (LRM 20.6.3) queries whether a parameter is the unbounded `$`; it belongs with the
  unbounded-parameter work noted out of scope in `hierarchy.md`, not with the shape queries here.
- The value-computation system functions -- `$clog2` (LRM 20.8), `$signed` / `$unsigned` (LRM 20.5),
  `$isunknown` / `$countones` / `$onehot` (LRM 20.9) -- compute over a value rather than query a
  type or shape. They are a separate family and are already supported.

## Cross-references

- `ibex.md` -- Q1 unblocks the `ibex_top` structural reduction gap.
