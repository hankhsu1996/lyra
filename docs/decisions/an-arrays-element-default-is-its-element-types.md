# What a position of an array holds before it is written is its element type's

Date: 2026-10-11 Status: accepted. Supersedes decisions 2, 5 and 6 of
[runtime-shape-and-default-value](runtime-shape-and-default-value.md) for every array kind, the
element default among a container's construction arguments in
[value-construction-forms](value-construction-forms.md) D1 and D2, the result default a locator and
`map` were handed under [array-manipulation-entry-stream](array-manipulation-entry-stream.md), and
the element default a queue concatenation carried under [queue-operators](queue-operators.md)
decisions 3 and 4. Takes over from
[a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) the statement of
an element type's default once per unit, widened from a fixed-size array to every array kind. D6
carries the same test to the other facts of a type a call was handed, and with it supersedes D2 of
[a-value-states-its-own-bits](a-value-states-its-own-bits.md) (a conversion out of a stream of bits
takes a prototype), a queue's bound as an operand of its construction in
[value-construction-forms](value-construction-forms.md) and among a queue concatenation's arguments
in [queue-operators](queue-operators.md) decision 3, and the empty reduction's element-shaped zero
in [array-method-dispatch](array-method-dispatch.md).

## Context

Reading an unpacked array of any kind through an invalid index answers a value LRM 7.4.5 fixes by
the array's element type alone (Table 7-1): x or 0 for an integral, "" for a string, null for a
handle, an array of no elements for a variable-size array, an array of such values for a fixed-size
one, and for an unpacked structure each member's value from the same table "unless the member has an
initial assignment as part of its declaration". A write through an invalid index does nothing, a
queue's `[$+1]` and a new associative entry excepted. The same value is what a new element of a
dynamic array holds (LRM 7.5.1), what a missing associative entry reads as (LRM 7.8.6), and what a
queue reads past its end (LRM 7.10.1).

Every call that built an array of a run-time size was handed that value as an operand: a
construction, `new[n]`, a conversion from one array kind to another, a concatenation's empty start,
and each array method answering a queue or another array. The array object kept a copy of it, and a
second copy for a discarded write to land on. A fixed-size array was handed none, since its type's
constant already named a function building its element's default
([a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) D7). So one
question had two answers, and each layer that met an array asked which kind it was before deciding
whether the operand was there.

Four more values were handed over the same way, each built where the call is from a type the call
already names: the default of a reduction's answer type, for a receiver holding no entries; the
default of the index type `$low` and `$high` answer, for an associative dimension holding none; the
default of the type a stream of bits is read into, as the value it took its shape from; and a
queue's declared bound, `-1` where it declares none, at every construction of a queue and every
conversion into one.

## The test each choice answers to

1. **An operand is what can differ between two evaluations.** A value that comes out the same every
   time control reaches a node is a fact of the node, stated once by the highest layer that knows
   it; handed over as an operand, it is evaluated again at every site for an answer that was settled
   before the program ran. Table 7-1 is a function of the type.
2. End-to-end iteration time is the primary target, so what is emitted at each site is weighed
   against what it buys.
3. Artifacts follow what one unit states, and a unit compiles from its own declarations, so that an
   edit recompiles only the units it reaches.
4. Memory follows the declared bits by at most a constant factor.
5. A backend entry is a fixed function of one MIR node. Where a predicate chooses between two output
   shapes, every consumer carries both, and the one it forgets is hidden by the other.

## What the field does

- libc++ builds a container's new elements by the element type's own default construction and never
  from a value the container was handed: `vector::__construct_at_end(size_type)` calls the
  allocator's `construct` on each new position with no value (`libcxx/include/__vector/vector.h`).
  The container object holds no default. Read.
- Rust has no implicit default, so `Vec::resize(new_len, value)` takes the fill as an operand
  (`library/alloc/src/vec/mod.rs`), and `resize_with` takes the function that makes one. Read.
- Go's runtime is handed the element type's descriptor where a slice is made and zeroes from it, and
  Swift's generic code is handed a type's value witness table. In both the type is handed over and
  the default is not. Recalled, not opened.
- Rust implements `Sum` and `Product` on the type summed into (`impl Sum for $a`), and what an empty
  iterator answers is part of that implementation (`library/core/src/iter/traits/accum.rs`); a
  caller of `iter.sum::<T>()` names the answer's type and passes no start value. Read.

SystemVerilog is on the C++ side of that split: Table 7-1 fixes the value by type, and Rust's
operand exists because Rust has no such table. The condition that differs from libc++ is the one
[a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) states: the
execution backend's library is compiled before any design exists, so "the element type's default
construction" reaches it as a function handed over with the type.

## Decision

**D1. What a position of an array holds before anything is written to it is a fact of the array's
element type.** It is the same for a fixed-size array, a dynamic array, a queue and an associative
array, so no layer tells the kinds apart to answer it.

**D2. A unit states it once per element type, in MIR.** For every type that is the element type of
an array type the unit holds, of any kind, the unit states the code that builds that type's default:
for a structure its declared default, member initializers included (LRM 7.2.2), for a variable-size
array its empty value, for a fixed-size array its elements' in turn. The unit states one for no
other type, and its verification holds it to exactly that set.

**D3. No call carries it.** A construction, `new[n]` and `new[n](src)`, a conversion from one array
kind to another, a concatenation, `map` and every locator state what can differ between two
evaluations -- the elements, a run-time count, the source, the closure -- and never what an element
of the answer starts as. Each operation therefore has one operand list whatever kind of array it
answers.

**D4. The prebuilt library is handed the element's type, and the default with it.** On the execution
backend a unit writes one constant per element type: the type, and the address of the function D2
states. An entry that builds an array is handed the type of the array it builds, before the storage
it builds into: for a fixed-size array and a queue the array's own type, which names the element
type's constant, since a fixed-size array's bytes state nothing and a queue's type states its bound
as well (D6); for any other kind the element type's constant, which the array it builds keeps the
address of. Which of the two is decided by the type answered, in one place, and an entry declares
only that it is told the array it builds. The two answers of LRM 7.4.5 -- the value an invalid read
returns and the storage a discarded write lands on -- are asked of the element type's constant,
whatever kind of array the position belongs to; each is built from the function the first time it is
asked for and kept beside the constant, once per element type per unit, for as long as the program
runs. On the C++ backend nothing is emitted for any of this: the host compiler's default
construction of the element type is the same statement, and each container asks it where a
fixed-size array already did.

**D5. The `default:` of an associative array literal stays an operand, because it is the one value
here that can differ where the type cannot** (LRM 7.9.11). "If a default value is specified, then
reading a nonexistent element shall yield the specified default value"; otherwise Table 7-1's is
returned. The value is one a program wrote for one array, and two arrays of one type may state
different ones. It is the operand of an operation of its own on the array the literal's entries
built, and not a second argument of that construction: a literal that writes none is the
construction alone, and nothing tells the two literals apart by counting what a construction was
handed.

**D6. Every other fact a type fixes reaches the library with that type too, and no call builds a
value whose only job is to show a type.** Test 1 decides each of these as it decides the element
default, since each comes out the same at every evaluation:

- **What an operation answers where it has nothing to compute from is its answer type's default.** A
  reduction (LRM 7.12.3 `sum`, `product`, `and`, `or`, `xor`) over a receiver holding no entries,
  which the clause leaves open, answers the default of the type it answers -- the element type's, or
  the type the `with` expression chose. `$low` and `$high` over an associative dimension holding
  nothing "shall return 'x" (LRM 20.7), which in a 4-state answer type is its default and in a
  2-state one becomes its default. Both answers are singular types (LRM 6.4), whose default Table
  6-7 fixes from the type alone, so the entry is told its answer's type and builds that default
  itself. An aggregate's default needs code the design states (D2), and no such entry answers one.
- **A conversion out of a stream of bits is told its answer's type and handed nothing else of it**
  (LRM 6.24.3). Its destination's stream has a shape the type fixes, a fixed-size array's count
  included, so the reader of each type takes the stream and writes the value; no value of the type
  is built for it to copy a shape from.
- **A queue's declared bound (LRM 7.10.5) is part of the queue's type.** On the execution backend a
  unit writes one constant per queue type, naming its element type's constant and its bound, and
  every entry that builds a queue or converts into one is told that constant (D4); on the C++
  backend the bound is a parameter of the queue's type. A bounded and an unbounded queue still have
  one construction, because neither is handed a bound. Making a value fit a destination of another
  bound is still an operation of its own, told the destination's type, since the source's bound is
  not the destination's.

## Rejected

- **"The canonical default flows from MIR lowering ... MIR type -> default-value expression ->
  wrapper-constructor argument -> stored `element_default_` / `discard_sink_` members. The shape
  information that is 'lost' at the C++ template-parameter level is restored via this explicit
  channel"** ([runtime-shape-and-default-value](runtime-shape-and-default-value.md) decision 6), and
  its rejection of a stored callable: **"Type-erasure overhead, no shape introspection from the
  stored callable, idiomatic mismatch with the `std::vector(n, value)` API"**. Both were derived
  under that record's decision 1, that a value's shape is a run-time property the C++ type has lost:
  "An empty container has no instances to query, so a generic OOB read has no shape source". That
  premise was withdrawn on 2026-10-04, when
  [a-value-is-its-machine-data](a-value-is-its-machine-data.md) lifted width, signedness and state
  domain to the type. Once the type states the shape, the type is the source an empty container
  asks, and an operand carrying the default restates it at every site. What D2 states is not type
  erasure over a value and has nothing to introspect: it is the type's default constructor, one per
  type, where the rejected callable was one per container object. `std::vector(n, value)` is the
  form for a caller that has a value to fill with; the form for a type that states its own start is
  `std::vector(n)`.
- **Carry the operand at every call and let the fixed-size array's entry ignore it.** Every site
  that builds a fixed-size array then builds a whole default value nothing reads, and an entry
  advertises a parameter it must document as unused, which no reader of the entry can tell from one
  whose contents matter. The C++ class for the fixed-size array would accept and drop an argument,
  or one array method would need a second form chosen by the kind of array it answers.
- **Omit the operand only where the answer is a fixed-size array.** The lowering then branches on
  the answer's kind at each of the places that build an array, the table of library entries carries
  a property that holds for some answers of one entry and not others, and whatever checks a call
  against its entry needs the same exception. That is one question answered in several places with a
  predicate choosing between two shapes, and test 5 rejects it. It rests on a gap, "a growable array
  is not told its type", which nothing requires.
- **The library working out an element's default from the type it is handed.** It cannot, for the
  reason [a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) gives: a
  structure's default holds its members' declared initializers, and only code compiled for the
  design knows them. A singular type's default is a different case, and D6 has the library build it.
- **"A type reference would not serve: the runtime holds a shape as a value, so an aggregate's
  element widths and state domains are reachable only through an instance of it"**
  ([a-value-states-its-own-bits](a-value-states-its-own-bits.md) D2), the reason a conversion out of
  a stream of bits was handed a prototype. Its premise is the one
  [a-value-is-its-machine-data](a-value-is-its-machine-data.md) withdrew: a value's width,
  signedness and state domain are its type's, and once a fixed-size array's type stated its count
  the prototype carried nothing any reader of it read. Every reader took it and ignored its
  contents: a parameter advertised and unread, which `../architecture/backend_contract.md` names as
  the cost of a value passed for its type alone.
- **A queue's bound as an operand, `-1` where it declares none**, which
  [value-construction-forms](value-construction-forms.md) chose because "a value the target spells
  has to be materialized by the IR, or the backend is composing it". A bound the type states is not
  composed by a backend: it is read from the type through type mapping, as a fixed-size array's
  count is, and stating it at every construction evaluates again at every site an answer settled
  before the program ran.

## Consequences

- An array object of a run-time size holds no copy of its element's default. It held two
  element-sized values per object, so an empty queue of a large structure cost two structures.
- Every site that built such an array built a default value of its element type first, and now does
  not.
- A unit states a default for every type it holds an array of, where it stated one only for the
  element types of its fixed-size arrays.
- One operand list per operation holds for every array kind, so the lowering, the table of library
  entries and the check of a call against its entry carry no case for the kind of array answered.
  The one place that tells the kinds apart is the one that decides which type an entry building an
  array is told (D4), and it decides from the type answered.
- A reduction, `$low` and `$high` over an associative dimension, and a conversion out of a stream of
  bits build no value of their answer's type where they are called, and a queue's construction and
  every conversion into a queue take no bound. A unit states one more constant per queue type.
- No speed or memory figure was taken.

## Cross-references

- [a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) -- the array
  type's constant, which names the element type's constant this record's D4 describes.
- [runtime-shape-and-default-value](runtime-shape-and-default-value.md) -- the default a container
  was handed and kept, which this supersedes.
- [value-construction-forms](value-construction-forms.md) -- a container built from an element list
  is a call; this says what that call no longer takes.
- [array-manipulation-entry-stream](array-manipulation-entry-stream.md) -- the algorithms' input;
  what a locator and `map` are told of their answer is this record's.
- [a-value-is-its-machine-data](a-value-is-its-machine-data.md) -- a value's shape is its type's,
  which is what removes the premise of the operand.
- [a-value-states-its-own-bits](a-value-states-its-own-bits.md) -- reading a value out of a stream
  of bits; D6 says what that conversion is told of its answer.
- [queue-operators](queue-operators.md) -- the queue's operators; D6 says where its bound lives.
- `../design-process.md` -- an operand is what can differ between two evaluations.
- `../architecture/backend_contract.md` -- an operand that stands for a type.
