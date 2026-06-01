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

U1 is in flight; U2..U5 are open and ordered. U1 establishes the IR shape and the literal-init path;
the rest extend on top.

| Item | Status                                                       |
| ---- | ------------------------------------------------------------ |
| U1   | In flight: type infrastructure + literal init + element read |
| U2   | Open: element write                                          |
| U3   | Open: structured and replicated assignment patterns          |
| U4   | Open: whole-array assignment, equality, constant-width slice |
| U5   | Open: OOB read returning element default, base ranges        |

## Sub-Steps

The numeric IDs are stable references and do not imply execution order beyond U1 -> U2.

### Type infrastructure and literal init

- [ ] U1 -- Fixed-size unpacked array declarations of integral element types (LRM 7.4.2), with
      element read on a known-in-bounds index (LRM 7.4.5) and declaration initializer via the simple
      assignment pattern `'{e1, e2, ...}` (LRM 10.9). Multi-dimensional arrays fall out from the
      nested type shape (LRM 7.4.4). Default initialization without an initializer applies the
      element type's LRM Table 6-7 default. Procedural element write, structured / replicated
      patterns, whole-array ops, and OOB behaviour are out of scope and live under U2..U5.

### Element write

- [ ] U2 -- Procedural element write `arr[i] = v`, including NBA (`arr[i] <= v`) and
      compound-assignment forms (`arr[i] += v`). The lvalue path mirrors the rvalue element-select
      from U1 with non-const access. Closes the element-access half of the `unpacked_arrays` archive
      cases that compute results by populating the array procedurally and reading back.

### Structured and replicated patterns

- [ ] U3 -- Structured assignment pattern (`'{default: v}`, `'{i: v, j: v}`) and replicated
      assignment pattern (`'{N{v}}`) per LRM 10.9.2 / 10.9.3. Closes the `unpacked_array_constants`
      archive cases that depend on `default:` or index labels.

### Whole-array operations

- [ ] U4 -- Whole-array assignment `A = B` for compatible types (LRM 7.6), equality `A == B` /
      `A != B` (LRM 7.4.6), and constant-width slice `A[i +: c]` / `A[i -: c]` (LRM 7.4.5).
      Compatibility follows the LRM 7.6 rules: equivalent element types and identical element
      counts, with left-to-right element correspondence.

### OOB and ranges

- [ ] U5 -- OOB read returns the element type's LRM Table 6-7 default; OOB write is a no-op (LRM
      Table 7-1, LRM 7.4.5). Ascending range bounds (`int a [0:N]`) and negative-base bounds
      (`int a [-1:6]`) translate through the index-to-offset path correctly.

## Cross-references

- LRM anchors: 7.4.2 (Unpacked arrays), 7.4.4 (Multidimensional arrays), 7.4.5 (Indexing and
  slicing), 7.4.6 (Operations on arrays), 7.6 (Array assignments), 10.9 (Assignment patterns), Table
  6-7 (Default initial values), Table 7-1 (Value read from a nonexistent array entry).
- Archive items: `datatypes/unpacked/{unpacked_arrays, unpacked_array_constants}`, the unpacked
  subset of `oob_bounds`.
- Decision: `../decisions/unpacked-array-representation.md`.
- Unblocks: `control-flow.md` C9 / C10 (`foreach` over unpacked).
- Cross-references `display.md` DI7 (`%p` assignment-pattern format) for end-to-end test coverage of
  whole-array values.
