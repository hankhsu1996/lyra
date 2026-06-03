# Aggregate Data Types

Tracks the variable-size aggregate types on the current pipeline: dynamic array (`int arr []`),
queue (`int q [$]`), and associative array (`int m [string]`). Fixed-size unpacked arrays belong to
`unpacked.md`. Struct and union are also aggregate types per LRM 6.5, but their storage and method
concerns are unrelated to the variable-size storage problem and they will get their own file
(`struct_union.md`) when that workstream opens; this file's scope is the variable-size subset only.

Done when:

- `datatypes/general` archive items reproduce on the current pipeline.
- `control-flow.md` C10 dynamic-array / queue / associative-array subset unblocks.

## Actionable

Three independent type workstreams under one umbrella. Dynamic array is in scope first: its
element-access shape is closest to fixed unpacked (already complete in U1..U7), which gives the most
natural reuse for procedural read / write and aggregate operations. Queue and associative array
follow once dynamic array's storage and runtime conventions are settled and proven against tests.

| Item | Status                                                               |
| ---- | -------------------------------------------------------------------- |
| DA1  | Open: construction, element read, blocking element write, multi-dim. |
| DA2  | Open: NBA, compound element write, whole-array assignment.           |
| DA3  | Open: literal init and assignment patterns.                          |
| DA4  | Open: aggregate equality and inequality.                             |
| DA5  | Open: constant-width slice (subject to LRM check).                   |
| DA6  | Open: SV-callable method family (`.size()`, `.delete()`, ...).       |
| DA7  | Open: invalid-index handling.                                        |
| Q1.. | Not yet scoped: queue.                                               |
| A1.. | Not yet scoped: associative array.                                   |

## Dynamic Array

LRM 7.5: a dynamic array is an unpacked array whose size is established at run time via `new[N]`
(with an optional copy source) and may be reset to empty via `.delete()`. Default value is the empty
array (Table 6-7).

### Sub-Steps

The numeric IDs are stable references and do not imply execution order beyond DA1 -> DA2.

- [ ] DA1 -- Type infrastructure, construction, element read, and blocking element write, including
      multi-dimensional dynamic arrays. Covers declaration `int arr []` and `int matrix [][]` with
      empty default (LRM Table 6-7), `new[N]` construction with a runtime size expression,
      `new[N](other)` copy construction from another dynamic array of the same element type (LRM
      7.5), element read on an in-bounds index (LRM 7.4.5), and blocking element write `arr[i] = v`.
      Newly constructed slots default to the element type's LRM Table 6-7 value (recursively, so
      nested dynamic arrays default to empty). Multi-dimensional forms fall out from the recursive
      element-type representation; no dim-count-specific scope is needed. `foreach` over a dynamic
      array stays rejected because the existing `foreach` flat-index lowering is keyed on
      compile-time per-dim counts; the runtime-count adaptation is its own step (`control-flow.md`
      C10 dynamic-array subset). SV-callable methods, including `.size()`, are not part of DA1 and
      ship with the method family in DA6.

- [ ] DA2 -- Aggregate write paths. NBA element write `arr[i] <= v`, compound element write per LRM
      11.4.1, whole-array assignment `A = B` which grows A to B's size per LRM 7.6, and the NBA form
      of whole-array assignment. Element-level paths follow the lvalue surface from U2; whole-array
      paths follow U4 with size-takeover on the destination.

- [ ] DA3 -- Literal init and assignment patterns (LRM 10.9). Declaration initializer
      `int arr [] = '{e1, e2, ...}` where the pattern determines array size, procedural assignment
      `arr = '{...}` which resizes the destination, and the replicated form `'{N{v}}` with a
      compile-time count. Any LRM-defined structured shapes on dynamic arrays (for example
      `'{default: v}` once a base size exists) land here too.

- [ ] DA4 -- Aggregate equality. `A == B`, `A != B`, `A === B`, `A !== B` per LRM 7.4.6 and 11.4.5.
      Size mismatch yields 0 directly; equal sizes reduce element-by-element comparisons to a 1-bit
      result. X / Z propagation rules match U5. Multi-dimensional dynamic arrays compose through the
      recursive element type.

- [ ] DA5 -- Constant-width slice (read and write), subject to LRM confirmation. If the LRM permits
      the `+:` / `-:` indexed-part-select form on `T []`, the in-scope subset mirrors U6:
      compile-time width, runtime base, lvalue treated as a single assignment to the slice;
      direction and OOB semantics carry over from U6 / U7. If the LRM forbids slicing on dynamic
      arrays, DA5 closes by emitting the rejection diagnostic instead.

- [ ] DA6 -- SV-callable method family. `.size()` query (LRM 7.5.1), `.delete()` clears all
      elements, and any other LRM-defined methods on dynamic arrays, plus the LRM 7.10
      array-querying methods that apply (`$size`, `$dimensions`, etc.). Sets up the SV
      method-dispatch path for the aggregate types in this file; queue and associative array will
      extend the same machinery with their respective method families.

- [ ] DA7 -- Invalid-index handling. Read on an out-of-range index returns the element type's LRM
      Table 7-1 default; write on an out-of-range index is a silent no-op; X / Z bits in the index
      follow the same default-on-read, no-op-on-write contract. Mirrors U7 for fixed unpacked.

## Queue

Not yet scoped. Sub-steps Q1.. open when dynamic array's runtime conventions are settled and the
queue-specific method family (LRM 7.10: `push_back`, `push_front`, `pop_back`, `pop_front`,
`insert`, `delete(i)`) is ready to be sequenced.

## Associative Array

Not yet scoped. Sub-steps A1.. open when dynamic array's runtime conventions are settled. Key type
can be any singular type or `string` (LRM 7.8); storage is sparse; the iteration protocol
(`.first()`, `.next()`, `.exists()` -- LRM 7.9) has no analogue in the other two families.

## Cross-references

- LRM anchors: 7.4.5 (Indexing and slicing of arrays), 7.4.6 (Operations on arrays), 7.5 (Dynamic
  arrays), 7.6 (Array assignments), 7.8 (Associative arrays), 7.9 (Associative array methods), 7.10
  (Queues and array-querying methods), 10.9 (Assignment patterns), 11.4.1 (Assignment operators),
  11.4.5 (Equality operators), Table 6-7 (Default initial values), Table 7-1 (Value read from a
  nonexistent array entry).
- Archive items: `datatypes/general/*`.
- Unblocks: `control-flow.md` C10 (foreach over dynamic / queue / associative subset).
- Decision: `../decisions/runtime-shape-and-default-value.md` -- runtime shape is retained on
  `PackedArray`; collection wrappers carry an explicit `default_value_` member for OOB synthesis and
  resize-fill.
- Cross-cutting: `refactor.md` R2 -- value-typed structural fields uniformly wrapped for
  observability (the U8 analogue for these container types once they participate in event control,
  level-sensitive `wait`, `always_comb`, or continuous assignment).
