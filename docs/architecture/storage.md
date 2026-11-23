# Storage

Parts of this contract are not yet carried out. The rules below bind every layer regardless, and a
layer that cannot meet one refuses rather than approximating it, with one exception that answers
wrongly instead: an ordering method on a dynamic array builds a new generation of its elements
rather than writing into the slots already there (invariant 7), so a reference bound to an element
before the call goes on reading the generation it was bound into.

## Purpose

SystemVerilog decides which storage a second name may denote, and that decision is the language's
rather than the compiler's. This document states what has storage identity, what may alias it, what
the components of an aggregate and of a container are, what a reference carries, what a write does
to the storage it reaches and what it reports, and which storage a process may wait on.

The distinction it exists to hold: **an expression that can be assigned is not necessarily an entity
that can be aliased.** A packed part select is a legal assignment target and is not a legal `ref`
actual; an unpacked array element is both. Every rule below follows from taking that seriously.

## Owns

- The set of entities that have **storage identity**, and the rule that decides membership.
- That a storage entity **owns its value**, and that value semantics are kept by copying at
  assignment.
- The separation between an **access path**, which is evaluated and may locate something different
  each time, and a **storage identity**, which is resolved once and retained.
- What a **reference** carries -- where the value lies and what holds that storage -- and what it
  must go on denoting while the aggregate around it changes.
- For each **container** kind, what it holds (membership and order) apart from where its elements
  lie, and which operations outdate a reference into it.
- What a whole-aggregate assignment does to the identity of its components, and the **store
  boundary** at which an assignment takes the destination's declared type.
- What a read and a write of a **part** are: a read makes a value, a write lands where the part
  lies.
- What a **write reports**: the update of the variable or object it reaches, which part it reached,
  and whether that part changed.
- Which storage a process may **wait on**, and the separation of storage identity from the identity
  a write raises an update on and from the expression a process waits on.

## Does Not Own

- How long storage lives and who ends it, including what keeps a detached element, or an object a
  reference points into, for the length of a call ([`lifetime.md`](lifetime.md)).
- How a value's bits are represented. Extent and alignment are all a compiler needs of a
  representation; everything inside one is a backend and runtime realization.
- The nodes that state an access, a write in progress, a step on a reference and a lease
  ([`mir.md`](mir.md)), and the place vocabulary below them ([`lir.md`](lir.md)).
- How a wait decides that its expression changed -- its baseline, its edge -- and when a woken
  process runs ([`scheduling.md`](scheduling.md)).
- What a procedural continuous assignment does to what a variable shows
  ([`net_resolution.md`](net_resolution.md)); this document says only where a write it displaces
  lands.
- The operations of a container type and the library core they run over ([`mir.md`](mir.md),
  [`backend_contract.md`](backend_contract.md)).
- Compiler-internal identity -- typed ids, how a unit names another unit's entity, which entity
  holds architecture-bearing state ([`identity_and_ownership.md`](identity_and_ownership.md)).
- Resolving a hierarchical name to a typed endpoint
  ([`reference_resolution.md`](reference_resolution.md)).
- The structure and construction protocol of a class object ([`object_model.md`](object_model.md)).

## Core Invariants

1. **Storage identity is a closed set, and membership follows from whether the aggregate gives each
   component storage of its own.** A variable, a class property, a member of an unpacked structure,
   and an element of an unpacked array have it; nothing else does (LRM 13.5.2). Packed aggregates
   give their components bit ranges of one vector and a union gives its members one shared storage,
   so neither yields components with identity. _Consequence: the storage hierarchy stops descending
   at the first packed aggregate and at any union, while the value hierarchy descends to bits._

2. **Assignability and storage identity are different questions.** A component with no identity is
   still writable, and the write is a write to the entity that contains it. _Consequence: no
   consumer may use left-hand-side position as the test for what may be aliased._

3. **A storage entity owns its value, and a component with identity is storage of its own.** A
   second name for the entity aliases its storage, never a handle to a value living elsewhere. A
   write to a component lands in that component and a read answers it where it lies; nothing is
   rebuilt around a part to change it. Value semantics are kept by copying at assignment -- an
   aggregate assigned copies its components, so a write through a reference to one is seen through
   no copy -- not by making a representation immutable. _Serves LRM 13.5.2, which makes a change
   through a reference visible to caller and callee at once, and the bound that a write costs what
   it reaches: a whole rebuilt to change a part costs the size of the variable, and a reference
   bound to a part of it writes back a stale whole._

4. **An access path is evaluated; a storage identity is retained together with what holds it.** A
   reference resolves its path once, at the bind, and holds where the value lies, what holds that
   storage -- the variable it belongs to, the object a property belongs to, or nothing -- and
   whether it names the whole of the variable. A part is lent by steps taken on a reference to its
   whole, each evaluated once at the bind. A write through a reference is a write of that holder at
   the moment it lands, reported as every write of it is. _Serves LRM 13.5.2 ("changes are seen
   outside the subroutine immediately") with LRM 4.3, which makes the change an update event on the
   variable, and LRM 9.4.2, which makes a property's change one on its object. Consequence: nothing
   that survives a bind carries a coordinate, because re-evaluating it later is not the same
   question; and only what acts on the whole variable -- its sampled value -- asks whether a
   reference names the whole._

5. **Forming a reference or a designation is an operation on the aggregate, and it reports what
   forming did.** Binding to an absent associative entry allocates it before any read or write
   through the reference (LRM 7.8.7), which is a change of the variable at the bind. A `ref` actual
   is formed at the call's entry; an `output` or `inout` actual at its write-back, which is an
   assignment. A read of an absent entry allocates nothing (LRM 7.8.6). _Consequence: a lowering may
   not treat formation as free of effect or reorder it with an observation of the container. A
   designation applied later -- a nonblocking assignment's -- is formed with no effect, because LRM
   10.4.2 bars a nonblocking assignment to an element of a dynamically sized array, the only family
   whose formation allocates; one designation serves both timings while that clause stands._

6. **A container holds membership and order, and whether its elements have identities of their own
   follows from whether the language removes one element from its middle.** A queue and an
   associative array do (LRM 7.10.2.3, 7.9.3), and LRM 13.5.2 keeps "the specific array element"
   that was "removed from the array", so each of their elements is storage of its own that never
   moves while it is a member: a queue keeps the order of its elements' storage, and an associative
   array keeps its entries in index order where the language fixes one and in the order they were
   made otherwise (LRM 7.8.5). A wildcard index names its entry by its value with leading zeros
   removed, whatever the width of the expression that wrote it (LRM 7.8.1), and the key an entry
   keeps is that value with the integral type it was written in, its width read off the value, since
   the array's index type fixes none. Inserting anywhere outdates a reference only to what the
   insertion removes (LRM 7.10.3); removing an element ends its membership and not its storage while
   a call holds a reference to it; writing a key again after its entry was removed makes a new
   entry, a second identity under the same key. A fixed-size array and a dynamic array remove no
   single element, so an index names one slot for the array's life: a dynamic array's slots are one
   generation of fixed size, replaced whole -- never resized in place -- by exactly what LRM 13.5.2
   says outdates a reference into it, a resize, a whole assignment and `delete()`. Every traversal
   -- indexing, iteration, `foreach`, formatting, an array method -- reads membership, so a detached
   element or a replaced generation is unreachable through the container by construction. _Serves
   LRM 7.10.3 and 13.5.2, and the bound that an ordinary access to an array is a direct index:
   identity bought with an order between index and storage is paid on every access, and only a
   container with removal from the middle needs it._

7. **An ordering method rewrites what an array's slots hold and reorders a queue's elements.**
   `sort`, `rsort`, `reverse` and `shuffle` on a fixed-size or dynamic array write values into the
   slots already there, outdate no reference, and leave a bound reference reading what its slot now
   holds; on a queue they reorder its elements, and a bound reference follows its element. LRM
   7.12.2 does not say which of the two an array's reordering is, and LRM 13.5.2 names no ordering
   method among what outdates a reference; this reading makes an ordering method do what a whole
   assignment of the array already does, move values and not entities (LRM 7.6).
   [`language_conformance.md`](language_conformance.md) holds the ruling.

8. **A whole-aggregate assignment writes into the existing components of a fixed-size aggregate,
   replaces the population of a variable-size one, and matches positions, not declared indices.**
   The first is element-wise into the components already there (LRM 7.6), so references to them stay
   valid and observe the new values; the second resizes or clears first, which is why the language
   outdates references there and nowhere else. Every whole-value movement -- assignment, copy,
   argument, equality -- pairs elements left to right, so `a = b` with `int a[7:0]` and `int b[1:8]`
   assigns `b[1]` to `a[7]`. The declared range is the static type's and is read only where a
   coordinate becomes a position; no value carries one. _Serves LRM 7.6. Consequence: whether a
   whole-value store may replace a component-bearing object is decided by fixed versus variable
   size, never by the store being whole._

9. **An assignment takes the destination's declared type at the store; a value movement adopts the
   whole value.** A semantic store -- an assignment, an initialization, a port connection, a
   write-back -- converts its source to the destination's declared type, which the lowering states
   as a conversion, and writes storage that keeps its declared type. A copy, a move or a returned
   value carries the value's own type with no conversion, because a value is pure: no value type's
   assignment keeps a shape of its own. _Serves LRM 10.7 and 10.8, which convert an assignment's
   source to its target's type; a semantic store reaching for the bare whole-value assignment would
   carry the source's type into the variable._

10. **A read of a part makes a value; a write of a part lands where the part lies.** A slice read
    gathers the positions it names into a value of its own, an out-of-range position read as the
    element type's default and the whole slice so where the base holds x or z (LRM 7.4.5, Table
    7-1), with those cases inside the operation. An invalid element read answers a value no write
    reaches and an invalid element write lands where no read reaches -- two different things -- and
    a write while a procedural continuous assignment holds the variable lands where nothing reads it
    (LRM 10.6). Bits of a packed value are written where they lie, only those named and in range
    (LRM 11.5.1), so the other bits of a member, x included, stay what the vector holds (LRM 7.2.1).
    An untagged union's member is a view of the union's one storage, read as the positions it takes
    and written by a store of them. A string's character lies inside the string's value and is read
    and written by the string's own operations, and a string holds no zero byte: a write of zero to
    a character is ignored and a conversion from an integral value drops every zero byte (LRM 6.16).
    A packed value printed with `%s` is read as its own bytes, never through a string converted from
    it ([`language_conformance.md`](language_conformance.md) holds what a zero byte prints as). The
    owner and each selector are evaluated once. _Serves LRM 7.4.5 and 11.5.1, and the bound that a
    write costs what it names ([`lir.md`](lir.md) invariant 10). A slice may hold positions with no
    storage, so no view of one has an address to keep._

11. **A reference aliases and does not own.** The language extends a lexically enclosing scope to
    cover the processes a `fork` spawns (LRM 6.21), and makes the program illegal where that would
    not reach (LRM 9.3.2); it never makes a reference an owner. What keeps a detached element, a
    replaced generation or an object for the length of a call is the binding that lent the reference
    ([`lifetime.md`](lifetime.md)), and the element going on existing is a property of the element
    (LRM 13.5.2). _Consequence: copying a reference, reading or writing through it, handing it to
    another formal and building a wait from it keep nothing alive._

12. **Storage identity, update identity and event identity are three separate things.** A write
    raises one update, on the declared variable it reaches (LRM 4.3), or on the object for a class
    property (LRM 9.4.2), at whatever depth it lands, and never one per component on its path: LRM
    13.5.2's own list puts a component beside a variable rather than calling it one. The update says
    which part of the variable the write reached. Which variable or object a write updates is a fact
    of the place written, stated by the compiler; storage holds no link to what contains it and
    discovers no observer. An event control waits on an expression and observes an event only where
    its result changes (LRM 9.4.2), and that filter and the result it compares against belong to the
    wait; an implicit sensitivity has no result to compare and is woken by a change to a part it
    reads (LRM 9.2.2.2.1), which is why a write says which part it reached
    ([`scheduling.md`](scheduling.md)). _Consequence: giving a component storage identity obliges
    nothing to give it a subscriber._

13. **A write to storage that reports its writes compares what it reached, and tells each observer
    it may have affected.** A write to a class object's property is the exception: the object is one
    event source for every property, so that write compares nothing and is told as it lands, and an
    event control on the property compares its own last value (invariant 14,
    [`scheduling.md`](scheduling.md)). Every other such write keeps the value of the part it lands
    on, not of the variable, and compares it when the write is over, holding both the value it
    replaced and the one it left so it answers once for every wait on that storage. It compares
    bits, never by `===`: `-0.0` written over `+0.0` is a change, and a NaN written over the same
    bits is none, since the update is a change in what the storage holds (LRM 4.3), which `===`
    misses in the first case and invents in the second (LRM 11.4.5). A step forming that part tells
    the write what forming did -- an associative entry made by being written (LRM 7.8.7), a queue
    element appended one past the end (LRM 7.10.1), an index naming nothing, which makes the write
    one that is ignored (LRM 7.4.5) -- since the part cannot tell an element it was from one the
    write made. A slice write answers the positions it changed; bits of a packed value are
    designated within the write, which keeps only the words they lie in; a whole-value store
    compares the new value with the old before writing and keeps a copy only where a wait watches a
    part inside it. A write tells each wait on a part inside the part it reached whether that part
    changed, stepping through the type's own description of its parts; where it kept no value to
    compare there -- inside a part it replaced, an element made or ended, an element of an array
    whose size the run sets written through a reference naming no position, a union's member, an
    associative array's entry -- it says the part may have changed. Every observer the write may
    have affected is told, since one left untold is never asked. What a write tells is which
    observers it may affect, never that a value is another, so an observer that answers for a change
    of value compares what it reads itself: an event control its expression (LRM 9.4.2), a wait on a
    size or on which object a class variable names what it last read, a foreign value-change
    callback what it last read (LRM 38.36.1). How the telling reaches each wait is
    [`scheduling.md`](scheduling.md)'s. _Serves LRM 4.3, which an implicit sensitivity with no value
    of its own to compare depends on exactly (LRM 9.2.2.2.1), and the bound that a write costs the
    part it reaches, not the variable._

14. **Which storage a process may wait on is decided by its declaration, and lending it changes
    nothing.** Every variable of a body that can wait, and every variable of static lifetime -- a
    design element's, a package's, a class's static property, a static a subroutine declares -- is
    storage that reports its writes, because any process that reaches a variable may wait on it (LRM
    9.4.2), and a body's variable is reached by the branches the body forks and the subroutines it
    lends the variable to (LRM 6.21, 13.5.2). A function cannot wait (LRM 13.4.4), so its automatic
    variables are plain values, as is every local the compiler makes. A class property is plain
    storage, and its object is the event source every property of it shares, since LRM 9.4.2 has a
    change to a data member reevaluate whatever reached the object. A declared variable's source is
    reached by a fact the compiler states; an object's is made the first time something waits on it,
    and an object nothing waits on carries none. _Serves LRM 9.4.2, which lets any variable be
    waited on, and north star invariant 5 ([`north_star.md`](north_star.md)): another unit may wait
    on a published variable, so "nothing waits on this" cannot be answered where the declaration is
    lowered. The two kinds of source are realized apart because their populations differ -- declared
    variables are fixed and mostly watched, objects are unbounded and almost never watched -- while
    nothing above the realization asks which kind it holds._

## Boundary to Adjacent Layers

- **MIR** carries access as place expressions, each stating the kind of storage it names -- one with
  an identity a reference may hold, or one with none -- so invariants 1 and 2 are settled where the
  node is built ([`mir.md`](mir.md) invariant 16). A write into a capability wrapper is opened on
  the wrapper, and each step into a part is taken within it, so the write hears what forming each
  part did (invariants 5 and 13). A reference is built over the whole the source names and each step
  into a part is taken on the reference; the binding takes the lease [`lifetime.md`](lifetime.md)
  describes. Reading what a wrapper holds and replacing the whole of it are calls on the wrapper;
  naming the storage is a dereference.
- **LIR** carries the place vocabulary and the operation that turns a place into a value. The phase
  boundary of invariant 4 lands there: the place is the access path and is consumed where it is
  written, and the value address-of yields is the retained identity. An element and a component are
  steps of a place, and bits of a packed value a slice step ([`lir.md`](lir.md)).
- **Each backend** realizes a storage identity in its own terms and answers, per storage kind, how a
  load through it, a store through it, and lending it are performed
  ([`backend_contract.md`](backend_contract.md)). A container's per-call operations are bodies the
  lowering builds for its type over a core the library compiles once ([`mir.md`](mir.md)).
- **[`lifetime.md`](lifetime.md)** takes over at duration: this document says a detached element
  goes on existing, that one says what keeps it and which regime ends it, and why an object a
  reference points into does not move.
- **[`scheduling.md`](scheduling.md)** takes over at the wait: how an event control compares its
  expression, and when a woken process runs.
- **[`language_conformance.md`](language_conformance.md)** holds the rulings where the standard
  leaves a reading open: what an ordering method does to an array's slots, and the identity of an
  associative entry made again under a removed key.

## Forbidden Shapes

Each names the requirement it fails.

- **A retained reference that stores a coordinate into a container** fails LRM 7.10.3: positions
  move while identities do not, so the reference would follow the position and denote another
  element.
- **Left-hand-side position used as the test for what may be aliased** fails LRM 13.5.2's list: it
  admits every packed part select, none of which is an entity.
- **Storage identity given to a component of a packed aggregate** fails LRM 7.4.1: the aggregate is
  one vector and the component a range of its bits, so two components can overlap and neither is
  addressable alone.
- **Storage identity given to a member of a union** fails LRM 7.3: a union is one storage whose
  members are readings of it, so distinct member identities would alias.
- **A whole-value store on a fixed-size aggregate that replaces the object holding its components**
  fails LRM 7.6 and 13.5.2: the assignment writes into those components, and replacing them leaves a
  reference to one denoting storage the program can no longer reach.
- **A part written by rebuilding the whole and storing it back** fails invariant 3's bound and LRM
  13.5.2: the write costs the size of the variable, and a reference bound to a part of it writes
  back a whole read before the write.
- **A reference that keeps its referent alive** fails LRM 8.30: every copy of a reference -- one
  handed to another formal, a wait built from it -- would keep an object a weak reference is meant
  to see released, and each forward would pay a count. The premise is that references are copied
  where the language lets them be; a call's lifetime is kept by the binding instead.
- **A part lent as the landing of a write held for the call**, or a reference into a snapshot of the
  owner, fails LRM 13.5.2: the write reports when the call's full-expression ends rather than when
  it lands, and a write by anyone else to the same variable while the callee runs is lost.
- **A slice read realized as a view that outlives its statement** fails LRM 7.4.5 and 7.5.1: the
  window may hold positions no storage backs, and a dynamic array's generation may be replaced while
  the view is held.
- **A subscriber list on a component's storage**, or a link from it to what contains it walked at
  each write, fails invariant 12: the event a process waits on is a change of an expression's
  result, which storage cannot answer, and the variable a write updates is known from the place at
  compile time, so a walk answers at run time a question already answered. Telling a wait which part
  of the entity a write reached is not this shape: the waits stay with the entity, kept by the part
  each one watches, and an implicit sensitivity is owed that grain, its list being stated part by
  part (LRM 9.2.2.2.1).
- **A write that keeps a copy of the whole variable to compare** fails invariant 13's bound: its
  cost follows the variable, not the part written.
- **An update raised once per component on a write's path** fails LRM 4.3, which puts the update on
  the variable, and multiplies the notifications of a write by its depth.
- **A storage form chosen by whether anything waits on the variable**, read from the body or the
  program, fails invariant 14: LRM 9.4.2 lets any process reaching the variable wait on it, and
  another unit's wait cannot be seen where the declaration is lowered.
- **A storage form chosen by whether anything lends the variable** fails the bound that a use costs
  what it does: a constraint of lending allowed to choose how the place itself is represented is
  paid by every read and write of the place, which lend nothing. How a storage kind is lent is each
  backend's answer for that kind (Boundary, above), never a shape the place takes.
- **A table keyed by storage address on the write path of a declared variable** fails the write
  path's cost: the source is reached by a fact the compiler states, and a declared variable is
  mostly watched. The premise is that measured population; the abstraction above the realization
  does not depend on it.
- **Observation state carried by every class object** fails the same cost from the other side: the
  population of objects is unbounded and almost never watched, so their sources are made on demand.
- **A storage hierarchy derived from a value hierarchy** fails invariant 1: the two descend to
  different depths, so the derived one invents identities the language withholds or drops ones it
  requires.
- **A semantic store realized as the bare whole-value assignment** fails invariant 9: the variable
  takes the source's type rather than keeping its own.

## Notes / Examples

The two hierarchies, on one declaration each:

```systemverilog
logic [31:0] p;
int a[4];
```

```
value hierarchy      p -> p[31] ... p[0]      a -> a[0] ... a[3]
storage hierarchy    p                        a -> a[0], a[1], a[2], a[3]
```

`p[3]` is a projection of one entity; `a[3]` is an entity. Reading a path outward-in, every layer
has identity until the first packed aggregate or union, and from there down nothing does -- so in a
struct holding `int m[4]` and `logic [31:0] q`, the entities are the struct, `m`, each `m[i]`, and
`q`, while `q[7:0]` is a projection.

Why invariant 4 is not a stylistic preference:

```
before push_front(X)     q = [A, B, C, D]        a reference denotes C, at q[2]
after  push_front(X)     q = [X, A, B, C, D]     the same reference denotes C, at q[3]
```

Every position changed and no identity did, so a reference holding `(container, index)` would have
begun denoting B.

What a reference carries, for each referent LRM 13.5.2 admits:

| Referent                                                       | What holds its storage                                      |
| -------------------------------------------------------------- | ----------------------------------------------------------- |
| a variable of a body that can wait, or of static lifetime      | the variable, whole                                         |
| an automatic variable of a function                            | nothing                                                     |
| a class property                                               | its object                                                  |
| a member or an element of a variable                           | the variable, as a part                                     |
| an element of a dynamic array, a queue or an associative array | the variable, as a part; the binding leases what it lies in |
| a `ref` formal handed on                                       | what that reference already carries                         |

The same method, two containers (invariant 7):

```systemverilog
int a[] = '{3, 1, 2};   // a reference r bound to a[0] reads 3
a.sort();               // a = '{1, 2, 3}; r reads 1, what slot 0 now holds
int q[$] = '{3, 1, 2};  // a reference s bound to q[0] reads 3
q.sort();               // q = '{1, 2, 3}; s reads 3, the element it was bound to, now at q[2]
```

**Current implementation.** LIR spells the access path as a place -- a base operand plus a
projection chain -- and address-of is the operation of invariant 4 that yields the retained
identity. A reference is a library object on both backends, holding what holds the storage, the
address, and whether it names the whole. A write in progress is a library object the writer opens on
the variable's cell, ends with its full-expression, and asks for the address each step designates. A
queue element's storage comes from the run's recycler for storage of its size, an associative
array's entries are each allocated on their own, and a dynamic array's generation is one block; each
keeps the count of the leases on it.
