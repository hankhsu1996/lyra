# Array manipulation over an entry stream

Date: 2026-06-22 Status: accepted

## Context

LRM 7.12 defines the array-manipulation method families that apply across the unpacked-array
containers: the locator family (`find` / `find_index` / `find_first` / `min` / `max` / `unique` /
...), the reduction family (`sum` / `product` / `and` / `or` / `xor`), the `map` projection, and the
ordering family (`reverse` / `sort` / `rsort` / `shuffle`). The first three apply to every unpacked
container including the associative array; the ordering family is defined only on the ordinally
indexed containers (LRM 7.12.2), and the frontend rejects it on an associative receiver.

The four containers differ along exactly one axis that the algorithms see: how an entry is indexed.

| Container         | Enumeration order   | Entry index | Index-locator result | `map` result          |
| ----------------- | ------------------- | ----------- | -------------------- | --------------------- |
| fixed unpacked    | declared position   | ordinal int | queue of int         | fixed unpacked of `U` |
| dynamic array     | position            | ordinal int | queue of int         | dynamic array of `U`  |
| queue             | position            | ordinal int | queue of int         | queue of `U`          |
| associative array | key order (LRM 7.8) | the key `K` | queue of `K`         | same-key assoc of `U` |

The locator / reduction / `map` scan logic is identical across all four: a predicate match, an
extremum, a first-occurrence pass, a fold, or a projection. Nothing in the scan branches on the
container. The only variation is (1) what an entry's index is, (2) what container the result shapes
into. Both are properties of the container, not of the algorithm.

The ordering family is a different requirement entirely: an in-place permutation of storage that
needs random-access exchange of element cells. It is not a function of an index stream, and it never
runs on an associative array.

## Decision

### Two abstractions, one per requirement cluster

1. **The locator / reduction / `map` families operate over an ordered entry stream** -- a lazily
   iterated sequence of `(index, element)` pairs in the container's natural order, modelled the way
   Rust's `Iterator` and C++ `std::ranges` model iteration. The index is a first-class value of the
   container's index type. The shared algorithm never synthesizes the index and never names a
   container type; each container exposes its stream as a lazy view (a sequence container is
   `views::enumerate` over its storage so the index is the ordinal; the associative array is a
   transform over its `std::map`, whose entries are natively `(key, value)`, so the index is the
   key). The algorithms are generic combinators over that view -- the locator family filters the
   entries (`find_first` adds `views::take(1)`, `find_last` adds `views::reverse`), `min` / `max`
   are `ranges::min_element` / `max_element` under a key projection, reduction is
   `ranges::fold_left` seeded by the first element, and `map` is a transform -- and each container's
   thin wrapper shapes the result.

2. **The ordering family stays an in-place positional permutation** on the sequence containers'
   storage. It is sequence-only; the associative array never participates, and the frontend enforces
   that. Rust draws the same line: `sort` lives on the mutable slice, not on `Iterator`.

The ordinal-index synthesis that the read-only algorithms once baked in (`int(i)` computed inside
the shared loop) lives only in the sequence containers' `views::enumerate`, not in any algorithm.

### The entry handle is a Regular value

An entry carries the element by `const`-pointer, not by reference. The algorithms feed entries
through `std::ranges` machinery, which assumes a Regular (copyable, assignable,
default-constructible) value; a reference member would forfeit Regularity (a reference cannot be
rebound) and cap which combinators an entry can flow through. The pointer is a non-owning handle
produced transiently by the view, never materialised into a stored collection.

### The producer supplies every result's default prototype

Every value-producing method receives the canonical default of its result's element (or scalar) type
as an argument, built by HIR-to-MIR from the call's result type. The result's runtime shape (bit
width, signedness, 2/4-state for an integral; the key shape for an index locator) is known at the
call site and is not always recoverable from the receiver: an index locator on an associative array
returns a queue of the key type, a `map` returns an element type the `with` expression chose, and a
reduction over an empty receiver must still carry the result's shape. One rule covers all three --
the producer supplies the result prototype -- so the runtime never self-derives a result shape from
the receiver and never fabricates a zero entry to recover one. The ordering family produces no new
value and takes no prototype.

### The closure index is the container's index type

The `with`-clause closure takes the iterator and its index (LRM 7.12.4 `item.index`). The index
parameter is typed as the container's index type: an ordinal int for the sequence containers, the
key type for the associative array. The runtime passes each entry's index straight into the closure,
so `item.index` reads the key on an associative receiver and the ordinal on a sequence receiver with
no special-casing.

## Rejected alternatives

- **A parallel associative algorithm family.** Duplicates the match / extremum / first-occurrence /
  fold / projection logic, which is provably identical across containers. The only differences
  (index type, enumeration source, result shaping) are container properties already expressible
  through the entry stream and the thin result wrappers. A second family is the parallel-structure
  shape the project forbids.

- **Injecting an index function into position-based loops.** Keeping the algorithms on a
  random-access `Seq&` and passing an `index_of(i)` closure fails for the associative map, whose
  `std::map` storage has no random-access-by-position. The honest shared input is the ordered entry
  stream, not a random-access sequence with an index side-channel.

- **Eager materialisation of the entry stream into a vector.** Building a `vector<Entry>` per call,
  then looping over it, is the "materialise then iterate" shape both Rust iterators and
  `std::ranges` deliberately avoid: it allocates a throwaway buffer and computes every index up
  front even when the consumer reads none. The stream is a lazy view instead, so a reduction that
  ignores the index never builds one and no buffer is allocated.

- **A key-shape prototype field on the associative container.** Carrying a `key_default_` so index
  locators could self-supply their result shape is unnecessary once the producer supplies result
  prototypes. An associative array's key shape is carried by its stored keys; any result that needs
  a key shape (an index locator's queue, including the empty case) receives it from the producer, so
  the container needs no extra key-shape state.

## Forbidden shapes

- A locator / reduction / `map` algorithm that assumes random-access storage or synthesizes an
  ordinal index. The index is an entry attribute the container supplies.
- A value-producing array method that derives its result's runtime shape from the receiver (copying
  the receiver's element default, or folding a fabricated zero entry to recover a shape). The result
  shape is producer-supplied.
- An associative array method that re-implements a 7.12 scan rather than feeding its entry stream
  through the shared algorithm.

## Notes

The empty-receiver reduction value (LRM 7.12.3 is silent) remains an element-shaped zero; under this
decision that zero is the producer-supplied result prototype rather than a copy of the receiver's
shield reset in place, which is the same value reached more directly. The sort-uses-selection-sort
and empty-reduction-is-zero runtime semantics pinned in
[array-method-dispatch](array-method-dispatch.md) are unchanged; this decision governs the algorithm
input shape, not those semantics.
