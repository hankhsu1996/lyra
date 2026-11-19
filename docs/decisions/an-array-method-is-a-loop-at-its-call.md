# An array method that asks something of each entry is a loop at its call

Date: 2026-06-22 Status: accepted. Revised 2026-10-17: the walk over the entries is compiled where
the call is rather than run by the library over a body it is handed, and a sort hands the library an
order rather than a key. Revised 2026-11-07: a sort hands the library its keys, compared by their
kind, and the array's type applies the order the library answers, which no array kind sorts by a
comparison it is handed any more (D2). The record is written as it now holds.

## Context

LRM 7.12 defines the array-manipulation method families that apply across the unpacked-array
containers: the locator family (`find` / `find_index` / `find_first` / `min` / `max` / `unique` /
...), the reduction family (`sum` / `product` / `and` / `or` / `xor`), the `map` projection, and the
ordering family (`reverse` / `sort` / `rsort` / `shuffle`). The first three apply to every unpacked
container including the associative array; the ordering family is defined only on the ordinally
indexed containers (LRM 7.12.2), and the front end rejects it on an associative receiver.

Every method but `reverse` and `shuffle` asks something of each entry: what a `with` clause answers
for it, or the element itself where the call writes none (LRM 7.12.4). The clause is an expression
of the program, of a type the program chose, reading the entry's element and index and anything else
in scope.

The four containers differ along exactly one axis the methods see: how an entry is indexed.

| Container         | Enumeration order       | Entry index          | Index-locator result | `map` result          |
| ----------------- | ----------------------- | -------------------- | -------------------- | --------------------- |
| fixed unpacked    | left bound to right one | declared index (int) | queue of int         | fixed unpacked of `U` |
| dynamic array     | position                | ordinal int          | queue of int         | dynamic array of `U`  |
| queue             | position                | ordinal int          | queue of int         | queue of `U`          |
| associative array | key order (LRM 7.8)     | the key `K`          | queue of `K`         | same-key assoc of `U` |

## Decision

### D1. A method is a loop over the entries, compiled where the call is

The locator, reduction and `map` families are a loop over the receiver's entries in their natural
order -- from the left bound for a fixed-size array, positions from the first for a sequence sized
while the program runs, keys in the order the index type imposes for an associative array -- with
the clause's expression evaluated at every entry as ordinary code of the body the call is in. The
element and the index are variables of that loop, under the clause's identity, so a clause nested in
another's expression reads the outer one's like any variable of the body. What a method does with
each key -- fold it, test it, compare it with the best so far, look for it among those already seen,
store it -- is code of the key's own type, so nothing is handed to a library to call back and
nothing crosses a library boundary as a type the library must be told.

The index is the entry's own index in the array (LRM 7.12.4): the key for an associative array, the
position for an array sized while the program runs, and for a fixed-size array the index its
declaration gives the entry, whatever the range's bounds and direction. So `item.index` and every
index locator (LRM 7.12.1) answer that index rather than how far the entry lies from the first, and
a `map` writes each answer at the entry's own index in an array of the receiver's range (LRM
7.12.5).

This is where the field puts it. Rust's `Iterator::sum` (`library/core/src/iter/traits/iterator.rs`)
is generic, and its generic MIR travels with the library so each use is compiled at its own types;
Verilator's `r_sum(T_Func with_func)`, in its runtime's `verilated_types.h`, is a template
instantiated at each element type and clause. Lyra's library is compiled before the program and
holds no template the program's types instantiate, so the loop is written by the compiler at the
call rather than instantiated from the library -- the same code, produced by the one party that
knows the types.

### D2. A sort hands the library its keys and applies the order it answers

A sort with a clause gathers each entry's key in a loop as D1 does; one without a clause takes the
elements themselves as the keys. The library answers the positions the keys come from once they are
in order, ascending for `sort` and descending for `rsort`, comparing the keys as the kind of value
they are: an integral value, a real or a string, the only types a relational operator orders (LRM
7.12.2, 11.4.4, 6.16), so the comparison is compiled once per kind and nothing is called back. The
array's type then puts its elements in that order by a body of its own: a fixed-size array through a
copy of itself, a dynamic array as a new generation of its elements, and a queue by its core
reordering where its elements lie, so no element of a queue moves. This is Rust's
`slice::sort_by_cached_key` (`library/alloc/src/slice.rs`), which computes each key once at the
caller and sorts positions by them.

The order is a merge over positions. The SV comparison over 4-state keys is not a strict weak
ordering -- a key carrying an `x` / `z` compares as not less either way while a definite `a < c`
still holds -- and a sort assuming one may read outside its range; a merge reads only within the two
runs it merges whatever the comparison answers, keeps equal entries in their order, and costs n log
n comparisons.

### D3. The answer's type is the call's

What a value-producing method answers is fixed by the call's result type and is not always
recoverable from the receiver: an index locator on an associative array returns a queue of the key
type, a `map` returns an element type the `with` expression chose, and a reduction over an empty
receiver must still answer a value of the result's type. The loop's result is a variable of that
type, starting at its default, which for a reduction is its answer for a receiver holding no entries
(LRM 7.12.3 states none): x for a 4-state type, 0 for a 2-state one. A reduction's first entry
replaces that default and each later one is folded into it, so the default never enters the fold.

## Rejected alternatives

- **A library algorithm handed the clause as a callback.** The callback answers a value of the
  clause's type, which the library compiled before the program cannot name, so every key crosses as
  a value of erased type and every fold, comparison and test is asked of a type record at run time.
  That moves typed work out of the compiler into a runtime that has to be told the types, which is
  the boundary this decision is drawn against.
- **A parallel algorithm family per container.** The differences between containers are what an
  entry's index is and which container a result shapes into; the loop takes both from the receiver's
  type, so one lowering covers all four.
- **A library entry per sort direction.** The direction is an operand of the one entry that orders
  the keys, which compares every kind both ways; a second entry would restate that comparison.

## Forbidden shapes

- A value-producing array method that derives its result's type from the receiver. The answer's type
  is stated by the call.
- A library entry that runs a body the program supplies once per entry and takes back a value of a
  type the program chose.

## Notes

`reverse` and `shuffle` ask nothing of an entry and stay the library's permutations.
