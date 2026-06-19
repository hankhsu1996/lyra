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

| Item | Status                                                                  |
| ---- | ----------------------------------------------------------------------- |
| DA1  | Done: construction, element read, blocking element write, multi-dim.    |
| DA2  | Done: NBA, compound element write, whole-array assignment.              |
| DA3  | Done: positional and replicated assignment patterns (LRM 10.9.1).       |
| DA4  | Done: aggregate equality, inequality, case-equality.                    |
| DA5  | Open: constant-width slice (subject to LRM check).                      |
| DA6a | Done: method dispatch + no-`with` subset.                               |
| DA6b | Done: `with` clause + iterator on sort / rsort / reduction methods.     |
| DA6c | Open: locator family (find\*, min, max, unique\*).                      |
| DA7  | Open: invalid-index handling.                                           |
| Q1   | Done: type, element read/write, native methods, default, case-equality. |
| Q2   | Done: operator surface (`$`, slice, concat, equality, bound, append).   |
| A1   | Done: string / integral index, element read / write, query methods.     |
| A2   | Done: traversal protocol (`first` / `last` / `next` / `prev`).          |
| A3   | Done: `foreach` over an associative array (any dimensionality).         |
| A4.. | Open: literals, whole-array assignment, locators.                       |

## Dynamic Array

LRM 7.5: a dynamic array is an unpacked array whose size is established at run time via `new[N]`
(with an optional copy source) and may be reset to empty via `.delete()`. Default value is the empty
array (Table 6-7).

### Sub-Steps

The numeric IDs are stable references and do not imply execution order beyond DA1 -> DA2.

- [x] DA1 -- Type infrastructure, construction, element read, and blocking element write, including
      multi-dimensional dynamic arrays. Covers declaration `int arr []` and `int matrix [][]` with
      empty default (LRM Table 6-7), `new[N]` construction with a runtime size expression,
      `new[N](other)` copy construction from another dynamic array of the same element type (LRM
      7.5), element read on an in-bounds index (LRM 7.4.5), and blocking element write `arr[i] = v`.
      Newly constructed slots default to the element type's LRM Table 6-7 value (recursively, so
      nested dynamic arrays default to empty). Multi-dimensional forms fall out from the recursive
      element-type representation; no dim-count-specific scope is needed. `foreach` over a dynamic
      array (including jagged nests) is handled by `control-flow.md` C10. SV-callable methods,
      including `.size()`, are not part of DA1 and ship with the method family in DA6.

- [x] DA2 -- Aggregate write paths. Compound element write per LRM 11.4.1, whole-array assignment
      `A = B` which grows A to B's size per LRM 7.6, and the NBA form of whole-array assignment.
      Element-level paths follow the lvalue surface from U2; whole-array paths follow U4 with
      size-takeover on the destination. NBA on a single element (`arr[i] <= v`) is excluded by LRM
      10.4.2 -- the NBA LHS rules forbid bit/element selects on dynamically sized targets, so the
      construct is rejected upstream by slang and produces no Lyra-side path.

- [x] DA3 -- Literal init and assignment patterns (LRM 10.9.1). Declaration initializer
      `int arr [] = '{e1, e2, ...}` where the pattern determines array size, procedural assignment
      `arr = '{...}` which resizes the destination, and the replicated form `'{N{v}}` with a
      compile-time count. Multi-dimensional forms fall out from the recursive element-type
      representation; jagged sizing per row is the natural consequence of each inner pattern
      determining its row's size. Structured forms with `default:` keys are rejected by slang for
      dynamic-array targets (LRM forbids `default` here); index-keyed forms are accepted by slang
      only when the indices cover a dense `0..N-1` set, which adds no expressive power over the
      positional form and is rejected at HIR lowering with a "use positional" diagnostic.

- [x] DA4 -- Aggregate equality. `A == B`, `A != B`, `A === B`, `A !== B` per LRM 11.2.2 + 11.4.5.
      LRM 11.2.2 requires equivalent operand type but is silent on runtime size mismatch; the
      industry-standard reading (Verilator-confirmed) yields 0 on size mismatch and 1 on
      empty-vs-empty (the identity of the element-wise `&&` reduction). For matched non-empty sizes,
      `==` / `!=` propagate X / Z through the per-element comparison; `===` / `!==` match X / Z as
      values and are deterministic. Multi-dimensional and mixed-container nesting (`int [3][]`,
      `int [][3]`) compose through the recursive element type.

- [ ] DA5 -- Constant-width slice (read and write), subject to LRM confirmation. If the LRM permits
      the `+:` / `-:` indexed-part-select form on `T []`, the in-scope subset mirrors U6:
      compile-time width, runtime base, lvalue treated as a single assignment to the slice;
      direction and OOB semantics carry over from U6 / U7. If the LRM forbids slicing on dynamic
      arrays, DA5 closes by emitting the rejection diagnostic instead.

- [x] DA6a -- Method dispatch infrastructure plus the no-`with`, no-queue-return method subset.
      `.size()` and `.delete()` (LRM 7.5.2 / 7.5.3); the LRM 7.12.2 ordering subset that takes no
      `with` clause (`.reverse()`, `.sort()`, `.rsort()`); the LRM 7.12.3 reduction subset that
      takes no `with` clause (`.sum()`, `.product()`, `.and()`, `.or()`, `.xor()`). Dispatch routes
      receiver-type-bound calls through the existing `BuiltinMethodRef` first-class home alongside
      enum / string / event methods; system-function array-querying counterparts (`$size`,
      `$dimensions`, etc. -- LRM 7.11 / 20.7) are a separate dispatch path and not bundled here.
      Slang gates element-type constraints upstream (integral element for reductions, comparable for
      sort).

- [x] DA6b -- `with`-clause variants of the methods that accept one (`sort`, `rsort`, reductions),
      plus the LRM 7.12.4 `item.index` iterator method. AST -> HIR consumes slang's
      `IteratorCallInfo` and binds the iterator HIR id; HIR -> MIR synthesises a `mir::ClosureExpr`
      using main's existing `CaptureSink` (so non-iterator outer reads in the body become
      by-reference captures automatically) and adds parameter bindings for `item` and `index`. The
      backend renders the closure as a lambda-with-captures and routes the call to the runtime's
      `*By` overloads.

- [ ] DA6c -- The locator-family methods that return an index or element queue (`find` family,
      `min`, `max`, `unique`, `unique_index`). The queue return container now exists (see Queue
      below). `shuffle()` (needs RNG model) and `map()` (SV2023, needs `with` infra + version gate)
      are standalone follow-ups after DA6c lands.

- [ ] DA7 -- Invalid-index handling. Read on an out-of-range index returns the element type's LRM
      Table 7-1 default; write on an out-of-range index is a silent no-op; X / Z bits in the index
      follow the same default-on-read, no-op-on-write contract. Mirrors U7 for fixed unpacked.

## Queue

LRM 7.10: a queue is a variable-size, ordered unpacked array with constant-time access and growth at
both ends. The index `0` is the first element and `$` the last; a queue may be bounded by an
optional right bound (`q[$:N]`).

- [x] Q1 -- Type infrastructure plus the element and native-method surface. Declaration `int q[$]`
      with the empty queue as default (LRM Table 6-7), element read and blocking element write on an
      ordinal index, the native methods of LRM 7.10.2 (`size`, `insert`, `delete` with and without
      an index, `pop_front`, `pop_back`, `push_front`, `push_back`), assignment-pattern
      initialization, whole-queue assignment, `%p` formatting (LRM 21.2.1.6), and the case-equality
      predicate used when a queue is an observable signal.

- [x] Q2 -- The operator surface that treats a queue as an unpacked array (LRM 7.10.1 / 10.10). `$`
      as the last-element index and as a slice bound (including `$-1` and friends); the slice in all
      three forms `q[a:b]` / `q[base+:w]` / `q[base-:w]` with the LRM 7.10.1 clamping and
      empty-result rules; unpacked array concatenation `{...}` including the empty queue `{}` (LRM
      10.10), which also expresses the push / pop / insert idioms of LRM 7.10.4; the aggregate
      equality operators (`==` / `!=` / `===` / `!==`); bounded-queue discard with a warning (LRM
      7.10.5); and the legal append write `q[$+1] = v` (LRM 7.10.1). One narrow form remains open:
      an unpacked concatenation whose result is a dynamic or fixed array rather than a queue.

## Associative Array

LRM 7.8: a sparse lookup table whose entries are allocated on first write. The index data type is
the lookup key and imposes an ordering.

- [x] A1 -- Type infrastructure plus the core element and query surface for string-indexed (LRM
      7.8.2, lexicographical order) and integral-indexed (LRM 7.8.4, signed/unsigned numerical
      order) arrays. Element read returns the element default for a nonexistent or invalid index
      without allocating (LRM 7.8.6); element write, including the read-modify-write compound form,
      allocates the entry with the element default first (LRM 7.8.7); an integral index carrying x/z
      is invalid, so a read returns the default and a write is ignored (LRM 7.8.6). Query methods
      `num` / `size` / `exists` / `delete` (LRM 7.9.1 -- 7.9.3), with the no-argument `delete`
      clearing the array. `%p` formatting prints entries in key order (LRM 21.2.1.6).

- [x] A2 -- The traversal protocol `first` / `last` / `next` / `prev` (LRM 7.9.4 -- 7.9.7), which
      assigns the visited key through a `ref` index out-parameter and returns 1 when an entry is
      found or 0 when none is (empty array, or no next / prev, leaving the index unchanged).
      Traversal follows LRM 7.8 key order: lexicographic for string indices, signed numerical for
      integral indices. The index write observes LRM 4.3, so an observable index variable fires its
      update event. The LRM 7.9.8 narrow-argument case (truncate the key into a smaller index
      variable and return -1) is unreachable: the frontend requires the index argument to be
      type-equivalent to the array's index type and rejects a narrower one before lowering.

- [x] A3 -- `foreach` over an associative array (`control-flow.md` C10), iterating the entries in
      LRM 7.8 index order (lexicographic for string keys, signed numeric for integral). The
      dimension walks by key via the traversal protocol rather than counting an index, and an
      associative dimension nests freely with index-counted dimensions in one foreach (associative
      of associative, associative of fixed, and the reverse).

- [ ] A4 -- Associative-array literals with an optional default (LRM 7.9.11) and whole-array
      associative assignment (LRM 7.9.9); the wildcard `[*]`, class, and other user-defined index
      families (LRM 7.8.1 / 7.8.3 / 7.8.5); and the locator-family methods that return an index
      queue of the key type (LRM 7.12.1).

## Cross-references

- LRM anchors: 7.4.5 (Indexing and slicing of arrays), 7.4.6 (Operations on arrays), 7.5 (Dynamic
  arrays), 7.5.2 (`size()`), 7.5.3 (`delete()`), 7.6 (Array assignments), 7.8 (Associative arrays),
  7.9 (Associative array methods), 7.10 (Queues), 7.11 (Array querying functions), 7.12 (Array
  manipulation methods), 7.12.1 (Locator), 7.12.2 (Ordering), 7.12.3 (Reduction), 7.12.4 (Iterator
  index), 10.9 (Assignment patterns), 11.4.1 (Assignment operators), 11.4.5 (Equality operators),
  Table 6-7 (Default initial values), Table 7-1 (Value read from a nonexistent array entry).
- Archive items: `datatypes/general/*`.
- Unblocks: `control-flow.md` C10 (foreach over dynamic / queue / associative subset).
- Decision: `../decisions/runtime-shape-and-default-value.md` -- runtime shape is retained on
  `PackedArray`; collection wrappers carry one `oob_slot_` member that doubles as canonical-default
  source and OOB-write discard target, reset via `T::ResetToDefault` on every OOB access.
- Cross-cutting: `refactor.md` R2 -- value-typed structural fields uniformly wrapped for
  observability (the U8 analogue for these container types once they participate in event control,
  level-sensitive `wait`, `always_comb`, or continuous assignment).
