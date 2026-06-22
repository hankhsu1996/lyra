# Unpacked Types

Tracks the fixed-size unpacked array surface on the current pipeline. Variable- size containers
(dynamic array, queue, associative array) belong to `datatypes/general` and have their own
workstream; unpacked struct and union belong to their respective archive items. The line of
separation follows the LRM type families, not archive layout.

Done when:

- `datatypes/unpacked/unpacked_arrays`, `unpacked_array_constants`, and `oob_bounds` (the unpacked
  subset) reproduce.
- `control-flow.md` C9 / C10 (`foreach` over unpacked) unblock.

## Actionable

U1..U9 are done. U8 (value-change observability) closed alongside `refactor.md` R2.

| Item | Status                                                         |
| ---- | -------------------------------------------------------------- |
| U1   | Done: type infrastructure + literal init + element read        |
| U2   | Done: element write (blocking, NBA, compound)                  |
| U3   | Done: structured and replicated assignment patterns            |
| U4   | Done: whole-array assignment (blocking and NBA)                |
| U5   | Done: array equality, inequality, case-equality                |
| U6   | Done: constant-width slice (read and write)                    |
| U7   | Done: invalid-index handling and non-canonical declared ranges |
| U8   | Done: unpacked vars participate in value-change observability  |
| U9   | Done: array manipulation methods (LRM 7.12)                    |

## Sub-Steps

The numeric IDs are stable references and do not imply execution order beyond U1 -> U2.

### Type infrastructure and literal init

- [x] U1 -- Fixed-size unpacked array declarations of integral element types (LRM 7.4.2), with
      element read on a known-in-bounds index (LRM 7.4.5) and declaration initializer via the simple
      assignment pattern `'{e1, e2, ...}` (LRM 10.9). Multi-dimensional arrays fall out from the
      nested type shape (LRM 7.4.4). Default initialization without an initializer applies the
      element type's LRM Table 6-7 default. Procedural element write, structured / replicated
      patterns, whole-array ops, and OOB behaviour are out of scope and live under U2..U7.

### Element write

- [x] U2 -- Procedural element write `arr[i] = v`, including NBA (`arr[i] <= v`) and
      compound-assignment forms (`arr[i] += v`, `arr[i] |= v`, etc., per LRM 11.4.1). The lvalue
      path mirrors the rvalue element-select from U1 with non-const access; NBA wraps the same
      lvalue inside the standard deferred-assign closure.

### Structured and replicated patterns

- [x] U3 -- Structured assignment pattern (`'{default: v}`, `'{i: v, j: v}`, and the mixed form) and
      replicated assignment pattern (`'{N{v}}`, including multi-item items and arbitrarily nested
      forms across multi-dimensional targets) per LRM 10.9.1 / 10.9.2. Closes the
      `unpacked_array_constants` archive cases that depend on `default:` or index labels.

### Whole-array assignment

- [x] U4 -- Whole-array assignment `A = B` for compatible fixed-size unpacked arrays (LRM 7.6).
      Element count must match; range bounds may differ, and correspondence between source and
      target elements is left-to-right per LRM 7.6 (`B[1] -> A[7]`, `B[8] -> A[0]` when `A` is
      `[7:0]` and `B` is `[1:8]`). Both blocking (`A = B`) and non-blocking (`A <= B`) forms are
      covered, with multi-dimensional targets composing through the nested element type.

### Array equality

- [x] U5 -- Array equality `A == B` and inequality `A != B`, plus case-equality `A === B` and
      case-inequality `A !== B`, over compatible fixed-size unpacked arrays (LRM 7.4.6 + 11.4.5).
      Result is a 1-bit value reduced from the per-element comparisons. For `==` / `!=`, X / Z in
      any element comparison propagates to the aggregate result per LRM 11.4.5; for `===` / `!==`, X
      / Z are matched as values and the result is always 0 or 1. Multi-dimensional aggregates
      compose through the recursive element type. Equality on slices of an array
      (`A[i +: c] != B[j +: c]`, also LRM 7.4.6) follows once U6 lands.

### Constant-width slice

- [x] U6 -- Constant-width slice `A[i +: c]`, `A[i -: c]`, and the direction-consistent constant
      range form `A[m:n]` over fixed-size unpacked arrays (LRM 7.4.5). Width `c` is a compile-time
      constant; base position `i` may be a runtime expression. Both rvalue and lvalue forms; the
      lvalue form is treated as a single assignment to the entire slice per LRM 7.6 and produces a
      single value change on the underlying array. The result type of an rvalue slice is itself an
      unpacked array. Slice equality (`A[i +: c] == B[j +: c]`) falls out from U5's recursive
      equality once the rvalue slice produces a wrapper value.

### Invalid index and non-canonical declared ranges

- [x] U7 -- LRM 7.4.5 invalid-index handling: a read with an out-of-bounds position or any X / Z bit
      in the index returns the element type's LRM Table 7-1 default; a write under the same
      condition is a silent no-op. Slice reads against a partially-OOB position yield in-range
      elements normally and defaults for the OOB portion; slice writes drop the OOB portion. Slice
      offset translation honours every declared range form -- ascending-from-zero, ascending with a
      non-zero base, descending, and negative-base -- so the in-memory offset of a slice's first
      element matches the slice's syntactic-leftmost SV position. Direction-mismatched constant
      ranges and entirely-OOB constant ranges remain frontend-rejected by slang's binding.

### Observability

- [x] U8 -- Unpacked structural variables participate in value-change observability. They are
      observable cells (`Var<UnpackedArray<T>>`), so `always_comb` / `@*` / level-sensitive `wait`
      reading an unpacked operand react to writes (a whole-array or element write fires the cell's
      any-change waiters), and a non-integral input port drives an unpacked child cell through the
      generic continuous-assign path. `@(arr)` on a whole unpacked array is frontend-rejected as a
      non-singular event expression (LRM 9.4.2); `@(arr[i])` on an element rides the shared
      value-change event-control path. (Selecting an unpacked element on the right-hand side of a
      structural continuous assignment is a separate structural-expression limitation, unrelated to
      observability.)

### Array manipulation methods

- [x] U9 -- Array manipulation methods (LRM 7.12) on fixed-size unpacked arrays: the ordering family
      (`reverse`, `sort`, `rsort`), the reduction family (`sum`, `product`, `and`, `or`, `xor`), and
      the locator family (`find` and its variants, `min`, `max`, `unique`, `unique_index`), with the
      optional / mandatory `with` clause per LRM 7.12.1 -- 7.12.3 and the `item.index` iterator (LRM
      7.12.4). LRM 7.12 defines these uniformly for any unpacked array (fixed or dynamically sized)
      except associative, so the semantics and the `with`-clause closure match the dynamic-array and
      queue receivers exactly; ordering mutates in place at the array's fixed size, and the locator
      family returns a queue. `shuffle` (needs RNG) and `map` (SV2023) remain the shared follow-ups
      noted under the aggregate workstream.

## Cross-references

- LRM anchors: 7.4.2 (Unpacked arrays), 7.4.4 (Multidimensional arrays), 7.4.5 (Indexing and
  slicing), 7.4.6 (Operations on arrays), 7.6 (Array assignments), 7.12 (Array manipulation
  methods), 10.9 (Assignment patterns), Table 6-7 (Default initial values), Table 7-1 (Value read
  from a nonexistent array entry).
- Archive items: `datatypes/unpacked/{unpacked_arrays, unpacked_array_constants}`, the unpacked
  subset of `oob_bounds`.
- Decision: `../decisions/unpacked-array-representation.md`.
- Cross-cutting: `refactor.md` R2 (done) -- unpacked fields are observable cells and react under
  `wait` / `always_comb` / `@*`, and a non-integral input port is admitted (U8).
- Unblocks: `control-flow.md` C9 / C10 (`foreach` over unpacked).
- `display.md` DI7 (`%p` assignment-pattern format) delivered alongside this workstream and powers
  the test framework's whole-array `expect.variables` assertion path; init / pattern / whole-assign
  / slice tests now assert against the entire array value.
