# A container operation asks the type or the values

Date: 2026-09-23 Status: accepted

## Context

An operation over a whole container -- imaging one across a foreign boundary, ordering the entries
of a keyed one, building one from a set of entries -- needs facts its own operands do not spell.
There are only two places to get one: the **declaration**, which the lowering still holds, and the
**values**, which are all the runtime has.

A monomorphized container never has to choose. Its type parameters hand it the element type and the
key's comparator, and its own body is compiled at those types, so a walk ends at the leaf type and a
comparison picks an order with nobody writing either down. A container realized once for every type
has neither, and then the choice is forced at every operation.

The question is which side each fact comes from, and the answer is not uniform. Deriving it as
though it were produced three wrong shapes in one day.

## The requirement

> An operation over a whole container must answer, at the moment it runs, every question its meaning
> depends on -- for a container holding no elements, for indices written at widths that differ, and
> for a call that arrives from outside the compiled program.

Those three clauses are not examples. They are the three cases where one of the two sides is empty,
and each selects a different source:

| The case                      | What the values cannot say | What the declaration cannot say |
| ----------------------------- | -------------------------- | ------------------------------- |
| The container holds nothing   | an element's shape         | --                              |
| The indices differ in width   | which of two comes first   | how wide one key is             |
| The call arrives from outside | --                         | nothing is in hand at all       |

## Findings that shaped the design

### F1. The standard puts the ordering on the index type and then withholds the type

LRM 7.8: "The data type to be used as an index serves as the lookup key and **imposes an
ordering**." For a string, a class, a chandle or a declared integral index, that ordering is also
what the index values already carry, so reading it off them agrees with the clause by coincidence.

LRM 7.8.1 ends the coincidence. A wildcard index admits any integral expression, makes it
self-determined and treated as unsigned, and orders the entries by numerical value -- so one value
may be written at any width and any signedness, and no index says what it means against another.

### F2. A type that fixes no representation is still a position values reach

The same clause gives a wildcard-indexed array no index data type. That is a statement about
representation, not about traffic: an index still travels to that position in an entry, and it is
always integral, because the clause admits nothing else. Reading "no data type" as "nothing of that
type is ever held" is what turns a one-line fact into an invented one.

### F3. A foreign caller holds no declaration

Annex H.12 has an open array reach the foreign side as a handle plus interface functions, and the
foreign code calls them. Nothing the compiler emitted is on that path, so whatever the operation
needs at that moment has to be answerable from the value.

### F4. Where the field's conditions match ours, it answers the same way

Verilator monomorphizes everything else and does not monomorphize this: its open-array handle is a
data pointer beside a descriptor carrying the element's type tag, its bit width and each dimension's
range, and the `sv*` functions are shared C code reading it. It has no erasure and still answers the
first case from the declaration.

Go's compiler emits one `abi.MapType` per map type, carrying the key type, the element type and a
hasher, which the shared runtime map code reads -- the key's discipline travels as data the
declaration produced, never on the keys.

CPython is the contrast: every object carries its type and an operation reads the comparison off the
value. That works exactly while a value is unambiguous about its own type, which LRM 7.8.1 is the
case that breaks.

**Where our conditions differ, and it is one place.** Go, Java and C++ have no keyed container whose
key type the language deliberately withholds. The second row of the table above cannot arise for
them, so the first and third rows take the field's answer outright and only the second is ours.

## Decision

**Each fact is taken from whichever side is guaranteed to have it, and a type that fixes no
representation still names what arrives at its position.**

### D1. A fact the declaration fixes and the values may lack crosses as an operand

The lowering holds the declaration, so it states the fact beside the operation. The canonical shape
an element is imaged at is one: an array with no elements has no element to read it off. The order
an index type imposes is another: the keyed container is built holding it.

### D2. A fact the values always have and the declaration does not fix is read off the value

A wildcard key's width is one. Whether a walk has reached a leaf is another -- an erased value
answers what domain it is, which is the same question a template asks of its type, asked of the
value instead. Neither is handed down, and handing either down would mean inventing it.

### D3. A position type is realized as what reaches it

A wildcard index type has a runtime realization -- the integral one -- even though it fixes no
width. This is what keeps one concept stated once: the container's key type and the key component of
its entries are the same type, and the widths live in the values underneath.

## Rejected alternatives

- **Widening every key of one assignment pattern to the widest among them**, so the entries share a
  representation. Built and reverted the same day. It invents the type the clause withholds, and it
  makes the compiler state two different things about one concept -- a container whose key type is
  the wildcard one, constructed from entries whose key component is a packed vector. The consumer
  that catches this is the monomorphizing backend, because its container's key type parameter comes
  from the same type the entries are listed under; the erased backend cannot see the disagreement at
  all. **The check that would have caught it first is asking what one concept is stated by, not
  whether each backend can translate what it is handed.**
- **Reading an element's shape off the container's first element.** What the monomorphized image did
  before this. Correct for every array that has one, and the empty case does not fall out of it.
- **Carrying the comparison on the keys.** What every other index type gets away with. LRM 7.8.1 is
  the case where the key is exactly the ambiguous thing, so no rule read off one can answer.

## Consequences

- A whole-container operation that is short of something asks which of the three cases it is in
  before it asks for a mechanism. Two of the three need nothing built.
- The empty case falls out of the general case wherever D1 holds: a shape stated by the declaration
  needs no element to exist, so the loop over elements runs zero times and no branch says so.
- A claim that nothing of some type is ever held is a claim about traffic, and the type system is
  not where it can be read. It is checked by finding the positions values reach.

## Cross-references

- LRM 7.8 (the index type imposes an ordering), 7.8.1 (the wildcard index), 7.9.4 -- 7.9.7 and
  7.12.1 (which withhold every way of reading a key back out), Annex H.7.3 / H.7.7 (an open array's
  canonical image), H.12 (the handle and its interface functions).
- [jit-aggregate-realization](jit-aggregate-realization.md) -- the erased realization that makes
  this choice forced rather than free.
- [dpi-open-array-boundary](dpi-open-array-boundary.md) -- the boundary object whose element shape
  D1 supplies.
- [a-types-readings-exist-because-the-type-does](a-types-readings-exist-because-the-type-does.md) --
  the same distinction from the other side: a type fixing no representation reads as no text.
