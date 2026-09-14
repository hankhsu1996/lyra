# An array element's identity is its slot, and a generation owns the slots

Date: 2026-09-11 Status: accepted

## Context

[container-element-storage](container-element-storage.md) settled the queue and the associative
array on element identity that survives a change of position, and expected the dynamic array to join
them. Checking that expectation turned up a silence in the standard that has to be answered first,
because it decides whether a _fixed_ array needs an order indirection as well.

**7.12.2, p.175** says array ordering methods "reorder the elements of any unpacked array (fixed or
dynamically sized) except for associative arrays", and describes each the same way -- `reverse()`
"reverses the order of the elements in the array", `shuffle()` "randomizes the order of the
elements". **13.5.2, p.349** lists five operations that outdate a reference, introduces them as "the
following operations **on a variable-size array**", and names no ordering method among them.

So after `a.reverse()`, with a reference bound to `a[0]`, the standard does not say whether the
reference follows the element to its new position or stays with the storage at index 0. Both
readings fit "reorder the elements": one reorders the entities, the other reorders what the entities
hold. No clarification, erratum or other authoritative text settles it.

This entry answers it as policy, and says so.

## The discriminator

What separates the containers that need element identity from those that do not is **whether the
language gives them an operation that removes a single element from the middle**:

| Container            | Single-element removal                              | Identity it needs |
| -------------------- | --------------------------------------------------- | ----------------- |
| queue                | `delete(index)` (LRM 7.10.2.3)                      | the element       |
| associative array    | `delete(index)` (LRM 7.9.3)                         | the entry         |
| dynamic array        | none -- `delete()` clears the whole array (LRM 7.5) | the slot          |
| fixed unpacked array | none -- its size never changes                      | the slot          |

13.5.2's detachment rule is what needs element identity, and it is written in those terms: "**the
specific array element** passed by reference shall continue to exist ... if those array elements
were **removed from the array**". A container with no mid-container removal gives that sentence
nothing to describe. That is why the split below is not two tastes; the two halves answer different
clauses.

## Decision

**A fixed unpacked array and a dynamic array give each index a persistent storage slot, and a
reference to an element binds that slot.**

**D1. Element identity is positional.** `ref a[i]` binds the element storage at index `i`. Within
one generation that slot persists for the array's life, so the reference stays valid and reads
whatever the slot currently holds.

**D2. Ordering methods permute values, not identities.** `sort`, `rsort`, `reverse` and `shuffle`
rewrite the values held by the existing slots. They move no storage, outdate no reference, and leave
every bound reference denoting the index it was bound to.

**D3. A dynamic array generation owns a fixed-size contiguous run of positional element storage.**
Its size never changes in place. In the common case the representation is a flat buffer and an
element reference is a pointer into it.

**D4. Resize, whole replacement and `delete()` create a new generation and outdate references into
the old one** -- which is exactly the set LRM 13.5.2 names for a dynamic array. If such a reference
is still live, the old generation is retained **whole** until that reference's required lifetime
ends.

**D5. This is Lyra policy where the standard is silent.** D1 and D2 are a choice among readings the
text permits, not a requirement it states. A conformance case may assert them only with this entry
cited beside it.

**D6. The queue and the associative array keep element identity and do not share this model.** The
expectation in [container-element-storage](container-element-storage.md) D6 that one element-storage
substrate would serve every container does not hold: the discriminator above puts the two array
kinds on the other side of it.

## Invariants

1. Within one generation, an index names the same storage object for the array's whole life. Nothing
   an ordering method does changes which storage an index names.

2. A generation's size is fixed at its creation. An operation that would change the element count
   creates a generation instead of resizing one.

3. Only the operations LRM 13.5.2 names for a variable-size array outdate a reference into a dynamic
   array. An ordering method is not one of them.

4. A retained old generation is unreachable through the array. Nothing indexes it, iterates it, or
   formats it; it exists only for the references that outlived it.

## Rejected

- **Element identity for arrays, the other reading of 7.12.2.** Under it, `a.reverse()` would have
  to move storage identities rather than values, which means even a **fixed** array needs an order
  indirection between index and slot -- a dependent load on every element access, and an ordering
  structure to maintain, for every array in the design. Rejected on consistency before cost: whole
  array assignment `a = b` already writes into the components that are already there
  ([storage-owns-its-value](storage-owns-its-value.md) D3, LRM 7.6), so the values move and the
  entities do not. An ordering method that moved the entities instead would make two operations with
  the same shape behave in opposite ways.

- **Per-element slot storage for the dynamic array, joining the queue's substrate.** It is what
  [container-element-storage](container-element-storage.md) D6 anticipated. Rejected because the
  dynamic array has no positional insert, so it never needs the property the slots were bought for,
  and would pay for them on every ordinary access: measured, a contiguous buffer reads at 1.717 ns
  on a dependent random access against 4.630 ns for a slot arena and 3.560 ns for a deque, and
  iterates at 0.068 ns against 0.846. The slot-arena shape exists to make positional insert cheap,
  and the dynamic array does not have one.

- **Per-element detachment for the dynamic array.** The finer-grained alternative to D4: retain only
  the referenced element rather than its generation. It removes the memory amplification below, and
  it requires the slot indirection the previous item rejects, so the two stand or fall together.

- **Making the two array kinds differ from each other.** A fixed array could keep slot identity
  while a dynamic array took element identity, since only the latter has generations at all.
  Rejected: nothing distinguishes them at the point a reference is bound, and an ordering method is
  defined over both by one clause.

## Consequences

- **A dynamic array is a flat buffer on the hot path.** Element access is a direct index, iteration
  is contiguous, and an element reference is a pointer into the buffer with no map and no
  indirection -- the fastest shape measured, and the one a native value model would want anyway.

- **One live reference retains a whole old generation.** A reference to one element of a
  32768-element array keeps all of it until the reference's lifetime ends. LRM 13.5.2 bounds that in
  time -- the called subroutine's scope -- and not in size. This is the accepted cost of D4 and the
  clearest thing that would reopen it: it is a slow-path memory spike traded against a permanent
  per-access indirection, which is the same trade this project has taken elsewhere.

- **`sort` is observably different between an array and a queue.** Binding a reference to `a[0]` and
  sorting leaves the reference reading the smallest value; doing the same to `q[0]` leaves the
  reference reading the element it was bound to, wherever it moved. One source-level method, two
  reference semantics, because the two containers answer different clauses. Both are conforming, the
  divergence is deliberate, and it is the one place a reader will expect uniformity and not find it.

- **A fixed unpacked array needs no ordering structure at all.** Its slots are permanent for the
  array's life, nothing outdates a reference into it, and `sort` rewrites values. It is the simplest
  container in the model.

- **The substrate of [container-element-storage](container-element-storage.md) D6 serves two
  containers, not four.** Stable element slots, detached lifetime and slot recycling remain one
  mechanism for the queue and the associative array; the arrays use generations instead, which reach
  the same requirements by a different route.

## Cross-references

- [container-element-storage](container-element-storage.md) -- the queue and associative array,
  whose D6 expectation this reverses for the arrays.
- [storage-owns-its-value](storage-owns-its-value.md) -- persistent per-element storage for the
  fixed array, and the whole-assignment rule this reads the ordering methods consistently with.
- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- why an element slot must have
  a real address, which the flat buffer of D3 provides directly.
- [unpacked-array-representation](unpacked-array-representation.md) -- the fixed array's
  representation this constrains.
- `../architecture/storage.md` -- identity independent of position is the language rule; this entry
  records where the language declines to say whether an ordering method moves the position or the
  value.
