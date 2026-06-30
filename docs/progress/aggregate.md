# Aggregate Data Types

Tracks the variable-size aggregate types on the current pipeline: dynamic array (`int arr []`),
queue (`int q [$]`), and associative array (`int m [string]`). Fixed-size unpacked arrays are a
separate, completed workstream out of scope here. Struct and union are also aggregate types per LRM
6.5, but their storage and method concerns are unrelated to the variable-size storage problem:
packed struct / union are tracked in `packed.md`, and unpacked struct / union are their own
separate, completed workstream. This file's scope is the variable-size subset only.

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
| DA5  | Done: constant-width slice (read and write).                            |
| DA6a | Done: method dispatch + no-`with` subset.                               |
| DA6b | Done: `with` clause + iterator on sort / rsort / reduction methods.     |
| DA6c | Done: locator family (find\*, min, max, unique\*).                      |
| DA6d | Done: `map` projection (LRM 7.12.5) on the sequence containers.         |
| DA6f | Done: two-name iterator / index argument form (LRM 7.12.4).             |
| DA6g | Done: `string` / `real` / `shortreal` as an unpacked-array element.     |
| DA7  | Done: invalid-index handling.                                           |
| Q1   | Done: type, element read/write, native methods, default, case-equality. |
| Q2   | Done: operator surface (`$`, slice, concat, equality, bound, append).   |
| Q3   | Done: array-manipulation method family (LRM 7.12).                      |
| A1   | Done: string / integral index, element read / write, query methods.     |
| A2   | Done: traversal protocol (`first` / `last` / `next` / `prev`).          |
| A3   | Done: `foreach` over an associative array (any dimensionality).         |
| A4   | Done: literals with optional default, whole-array assignment.           |
| A5   | Done: locator / reduction / map manipulation methods (key-indexed).     |
| A6   | Done: wildcard `[*]` and packed-struct index; class index deferred.     |

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

- [x] DA5 -- Constant-width slice (read and write). LRM 7.4.6 permits slicing on any unpacked array
      except associative, so a dynamic array slices like a fixed unpacked array: the constant width
      makes the result a fixed-size unpacked array, the position may be runtime, and a slice write
      (LRM 7.6) is a single assignment to the whole slice. All three bound forms (`a:b`, `+:`, `-:`)
      and multi-dimensional receivers are covered. Direction and out-of-range / x-z semantics carry
      over from U6 / U7: an out-of-range position reads the element default and is skipped on write,
      while an x / z position makes the whole slice default on read and a no-op on write.

- [x] DA6a -- Method dispatch infrastructure plus the no-`with`, no-queue-return method subset.
      `.size()` and `.delete()` (LRM 7.5.2 / 7.5.3); the LRM 7.12.2 ordering subset that takes no
      `with` clause (`.reverse()`, `.sort()`, `.rsort()`); the LRM 7.12.3 reduction subset that
      takes no `with` clause (`.sum()`, `.product()`, `.and()`, `.or()`, `.xor()`). Dispatch routes
      receiver-type-bound calls through the same built-in-method call mechanism that serves enum /
      string / event methods; system-function array-querying counterparts (`$size`, `$dimensions`,
      etc. -- LRM 7.11 / 20.7) are a separate dispatch path and not bundled here. Slang gates
      element-type constraints upstream (integral element for reductions, comparable for sort).

- [x] DA6b -- The `with`-clause variants of the methods that accept one (`sort`, `rsort`, and the
      reductions), plus the LRM 7.12.4 `item.index` iterator method. The `with` expression is
      evaluated as a closure over each element and its index; an outer variable read inside the body
      is captured by reference so the body observes live values, and an omitted `with` clause
      applies the LRM-default `with (item)`.

- [x] DA6c -- The locator-family methods that return an index or element queue (`find` family,
      `min`, `max`, `unique`, `unique_index`), including the optional `with` clause for `min` /
      `max` / `unique`. `shuffle()` consumes a simulation RNG and is tracked under
      `simulation-rng.md`.

- [x] DA6d -- The `map()` projection (LRM 7.12.5) with its mandatory `with` clause. Each element and
      its index pass through the expression into a result whose shape and index type match the
      receiver (dynamic array, queue, or fixed unpacked) and whose element type is the expression's
      self-determined type, so it may differ from the source (e.g. `int` to `bit`, `string`, or
      `real`). An empty receiver yields an empty result.

- [x] DA6f -- The two-name iterator / index argument form of the LRM 7.12 family
      (`a.method(item, idx) with (...)`, LRM 7.12.4), which renames the iterator and the index-query
      method to avoid clashes with member names of the stored elements. The frontend normalizes the
      form away: the renamed iterator flows through as the with-clause iterator name, and a renamed
      index method resolves back to the canonical index query, so the existing closure synthesis
      handles it with no new lowering. Shared across find / sort / map / reductions. The clash it
      exists to resolve only arises once an element type has its own members (struct / class /
      union), which the array-method family does not yet take.

- [x] DA6g -- `string`, `real`, and `shortreal` as unpacked-array elements (dynamic array, queue,
      fixed unpacked): declaration, element read / write, the element-type OOB default (LRM Table
      6-7: `""` for string, 0.0 for real), aggregate equality, `%p` formatting, and as a `map()`
      result element type. `string` only needed its canonical-default reset. `real` / `shortreal`
      first needed to become value types at all -- they were bare host `double` / `float` -- so the
      `lyra::value::Real` value type was introduced (see `../decisions/value-type-concepts.md`),
      after which the element support falls out through the same shield pattern as every other type.
      Case equality (`===` / `!==`) on a real or real-element aggregate is rejected in lowering: LRM
      Table 11-1 excludes real / shortreal from case equality.

- [x] DA7 -- Invalid-index handling (LRM 7.4.5). Read on an out-of-range index returns the element
      type's Table 7-1 default; write on an out-of-range index is a silent no-op; X / Z bits in the
      index follow the same default-on-read, no-op-on-write contract, as does every index into a
      zero-size array. Same contract as U7 for fixed unpacked.

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

- [x] Q3 -- The array-manipulation method family (LRM 7.12) on a queue receiver, available because a
      queue supports every unpacked-array operation (LRM 7.10.1). The ordering family (`reverse`,
      `sort`, `rsort`), the reduction family (`sum`, `product`, `and`, `or`, `xor`), and the locator
      family (`find` and its variants, `min`, `max`, `unique`, `unique_index`), each with the
      optional / mandatory `with` clause per LRM 7.12.1 -- 7.12.3 and the `item.index` iterator (LRM
      7.12.4). Locator methods return a queue of elements or of `int` per LRM 7.12.1; reduction
      result width follows the `with`-expression type. The semantics and the `with`-clause closure
      are the same as for the dynamic-array receiver. The `map` projection (LRM 7.12.5) is shared
      and done on a queue receiver too; `shuffle` is tracked under `simulation-rng.md`.

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

- [x] A4 -- Associative-array literals with an optional default (LRM 7.9.11) and whole-array
      associative assignment (LRM 7.9.9). The literal `'{index: value, ..., default: d}` initializes
      entries by key, and a `default:` entry installs a persistent default that a read of a
      nonexistent index returns with no warning; without it a read-miss returns the LRM Table 7-1
      default. The default is a property of the array, not just the initializer, and does not affect
      the associative methods (LRM 7.9). Whole-array assignment `A = B` between two associative
      arrays of the same index type clears the target and copies every source entry (LRM 7.9.9); an
      associative array is assignment-compatible only with an associative array.

- [x] A5 -- The associative-array manipulation-method family (LRM 7.12): the locator family (`find`,
      `find_index`, `find_first`, `find_first_index`, `find_last`, `find_last_index`, `min`, `max`,
      `unique`, `unique_index`), the reduction family (`sum`, `product`, `and`, `or`, `xor`), and
      the `map` projection, each with the `with`-clause and iterator-index forms shared with the
      dynamic-array and queue receivers. On an associative receiver the iterator index is the key,
      so an index-returning locator returns a queue of the key type rather than `int` (LRM 7.12.1),
      `item.index` inside a `with` clause is the key (LRM 7.12.4), and `map` yields a same-key
      associative array whose element type is the `with`-expression type (LRM 7.12.5). Traversal
      follows the array's index order (LRM 7.8). The ordering family (`reverse` / `sort` / `rsort` /
      `shuffle`) is excluded for associative arrays (LRM 7.12.2) and is not part of this step;
      reduction is included because LRM 7.12.3 permits it on any integral-valued unpacked array.

- [x] A6 -- The remaining associative index families. A wildcard `[*]` index admits any integral
      expression and identifies an entry by its unsigned numerical value, so two indices of the same
      value but different widths name one entry and ordering is by magnitude (LRM 7.8.1). The
      frontend rejects a wildcard-indexed array used in `foreach` or with a manipulation method that
      returns an index (LRM 7.8.1 / 7.12.1), so those illegal forms never reach lowering. A
      packed-struct key (LRM 7.8.5) is an integral index, keyed and ordered by its bit-pattern
      value. The class index (LRM 7.8.3) stays blocked on class support and is deferred to that
      workstream.

## Cross-references

- LRM anchors: 7.4.5 (Indexing and slicing of arrays), 7.4.6 (Operations on arrays), 7.5 (Dynamic
  arrays), 7.5.2 (`size()`), 7.5.3 (`delete()`), 7.6 (Array assignments), 7.8 (Associative arrays),
  7.9 (Associative array methods), 7.10 (Queues), 7.11 (Array querying functions), 7.12 (Array
  manipulation methods), 7.12.1 (Locator), 7.12.2 (Ordering), 7.12.3 (Reduction), 7.12.4 (Iterator
  index), 7.12.5 (Mapping), 10.9 (Assignment patterns), 11.4.1 (Assignment operators), 11.4.5
  (Equality operators), Table 6-7 (Default initial values), Table 7-1 (Value read from a nonexistent
  array entry).
- Archive items: `datatypes/general/*`.
- Unblocks: `control-flow.md` C10 (foreach over dynamic / queue / associative subset).
- Decision: `../decisions/runtime-shape-and-default-value.md` -- runtime shape is retained on
  `PackedArray`; each collection wrapper carries an immutable element-default prototype and a
  separate write-discard sink, so the read path is pure.
- Decision: `../decisions/array-manipulation-entry-stream.md` -- the LRM 7.12 locator / reduction /
  map family runs over an ordered `(index, element)` entry stream the container supplies (the key
  for an associative receiver), and the producer supplies every result's default prototype.
- Cross-cutting: `refactor.md` R2 (done) -- these container value types are observable cells and
  react under `wait` / `always_comb` / `@*`, and a non-integral input port is admitted (the U8
  analogue for these containers).
