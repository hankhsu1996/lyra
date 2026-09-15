# A container stores membership; its elements are stable storage of their own

Date: 2026-09-11 Status: accepted

## Context

[storage-owns-its-value](storage-owns-its-value.md) settled that a component the language gives
storage identity is itself a storage entity, and settled the two fixed-arity aggregates. It left the
variable-size containers open, because their elements need something the fixed ones do not: an
identity that survives a change of position, and storage that survives a change of membership.

What the language requires is established. A queue element's identity is independent of its position
-- inserting at any position outdates a reference only to an element the insertion itself removes
(LRM 7.10.3). An element removed from any variable-size container while a reference is bound to it
goes on existing for the called subroutine's lifetime, and writes through that reference are not
visible through the container (LRM 13.5.2). Binding a reference to an absent associative index
allocates the entry before any read or write of it (LRM 7.8.7). Associative traversal visits entries
in index order (LRM 7.8.2, 7.8.4).

What those rules have in common is that **a container's ordering and its elements' storage are two
different things**, and a representation that fuses them cannot satisfy both. The shape in use today
fuses them -- elements live in the container's own contiguous buffer, at positions the container
moves -- and that is both the measured wrong answer and the reason a reference to an element cannot
be expressed.

## Decision

**A variable-size container stores logical membership and ordering. Its elements live in stable
element storage the container does not move.**

**D1. Queue.** Queue elements live in stable element slots, and the queue stores the logical
ordering of those slots. A structural mutation changes the ordering, never the storage identity of a
surviving element. `q[i]` is therefore two steps -- resolve the order entry, then reach the slot --
and `push_front`, `insert` and `delete` move slot references rather than payloads.

**D2. Associative array.** The array maps keys to stable entry storage. Inserting another key does
not change an existing entry's identity. A key is the lookup handle, so no separate ordering
structure is needed -- which is the one way the associative case is simpler than the queue, where
the logical identity is not the index.

**D3. Removal separates membership from lifetime.** Removing an element removes its membership. If a
live reference still requires its storage, that storage is detached -- kept alive outside the
logical order until the reference's required lifetime ends -- and otherwise it is recycled.

**D4. Iteration and traversal walk membership, never storage.** A detached element is therefore
never visited, and no consumer ever skips a hole. This is what makes D3 affordable: detachment costs
the traversal path nothing, because the traversal path does not look at the arena.

**D5. Recreating an associative key creates a new entry identity.** After `a.delete("k")` with a
live reference to that entry, a later `a["k"] = v` allocates a new entry; the old detached storage
and the new entry are two identities that happen to share a key. The LRM does not decide this
(recorded as a silence in `~/wiki/systemverilog/lrm-open-questions.md`); this is Lyra's policy, and
it follows from D3 -- the detached element has already left membership, and 7.8.7 allocates on the
ground that the index has no entry.

**D6. One element-storage substrate, not one per container.** Stable element storage, detached
lifetime, and slot recycling are the same three mechanisms for every container kind, so they are one
mechanism rather than three reinventions. A dynamic array is expected to join it; that is not
decided here.

**The expectation about the dynamic array did not hold.**
[array-element-storage](array-element-storage.md) puts both array kinds on the other side of a
discriminator this entry had not identified: the containers that need element identity are exactly
those with single-element removal, which LRM gives to the queue and the associative array and to
neither array. The substrate serves those two.

## Invariants

1. A container holds membership and ordering. It never holds its elements' storage in a place it
   relocates.

2. A structural mutation changes membership. It does not change the identity, or the storage, of any
   element it does not remove.

3. An element that leaves membership while a reference requires it keeps its storage until that
   requirement ends. Nothing else keeps it.

4. Every traversal -- indexed access, iteration, `foreach`, formatting, a `with` clause -- reads
   membership. A detached element is unreachable through the container by construction, not by a
   filter.

5. An associative key is not an identity. Two entries under one key, at different times, are two
   entities.

## Rejected

- **A linked list of element nodes.** The obvious way to get stable elements and
  position-independent identity, and it gives up the thing SystemVerilog queues actually do: `q[i]`
  must stay efficient. Indexed access is a primitive of the type, not an occasional operation.

- **Keeping the current contiguous buffer and accepting the semantic gap.** This is the shape in
  place, and it does not merely lack a feature -- it answers legal programs wrongly. Measured on the
  C++ backend: a reference bound to a queue element behaves as a designator over the container value
  at the bind, so a callee's own `push_front` becomes invisible to the caller and a write through
  the reference restores the pre-push contents. It is invisible to the corpus because no case
  mutates a container while a reference to an element is live.

- **Element storage held directly in the map's nodes** (`map<Key, ElementStorage>`), for the
  associative array. Correct for identity, and what the C++ backend's `std::map` already provides --
  so this is rejected on a narrower ground than correctness. It puts the stability requirement on
  the map, which then cannot be changed without re-deciding identity, and it gives the associative
  array its own detachment machinery rather than the shared one of D6. **Its price is real and is
  accepted knowingly**: `map<Key, SlotId>` plus an arena costs one extra dependent load on every
  keyed access compared with storage in the node, about 1.1 ns by the reference measurements. One
  lifetime mechanism for every container is worth that; three are not.

- **Per-container detached-element machinery.** Each container inventing its own keep-alive is the
  same mechanism written three times, and the rule it implements -- LRM 13.5.2's scope bound -- is
  one rule.

- **Reference counting each element slot.** The straightforward way to keep a detached element
  alive. Rejected because 13.5.2 bounds the requirement to the called subroutine's scope, so a
  scope-bounded owner suffices, and a refcount would put its traffic on every bind and every release
  to pay for a case that is rare.

- **`boost::container::stable_vector` or an equivalent library container.** It provides stable
  element references across container mutation, which is most of D1 and D2, and not the part that is
  hard: an _erased_ element must itself survive. A library whose contract ends at "references to
  remaining elements stay valid" does not express D3.

## Consequences

- **The execution backend's associative array must change representation.** It is an index-ordered
  `vector` of entries today, so any insertion moves entries and invariant 2 fails. The C++ backend's
  `std::map` already satisfies identity and satisfies neither D3 nor D6.

- **The associative map must stay ordered, or become sortable on demand.** LRM 7.8.2 / 7.8.4 require
  traversal in index order, so moving stability out of the map into the arena does not make the map
  free to be an unordered hash table. It frees the map from the _stability_ requirement, not from
  the _ordering_ one.

- **A slot's nature is the open question inside this decision, and it decides the reference.** If a
  slot is an index into storage that may relocate, a reference needs the arena as well as the index:
  two dependent loads and a wider reference. If a slot is an address in storage that never
  relocates, the reference is that address: one load, eight bytes. Measured, the two are within 3%
  on element access and within a factor on mutation, so the reference is what should decide -- and a
  third combination, non-relocating slots named by a small id in the order array, would take the
  cheaper reference and the cheaper mutation together. That combination is unmeasured, and measuring
  it is the next step rather than a detail.

  **Answered by [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md): a slot is an
  address.** A reference is one machine word, and an index into relocatable storage needs the arena
  beside it, which is more than a word carries. What stays open is only how the ordering structure
  names a slot -- a pointer or a small id -- which the reference does not see.

- **The ordering structure's element size is a measured cost, not a free choice.** A middle insert
  moves the order array, so a 4-byte slot id and an 8-byte pointer differ by a factor of two in what
  a mutation moves: 159 ns against 209 ns at an 8-byte payload, and 128 ns against 226 ns at 168
  bytes.

- **The dynamic array is not settled.** Its candidate -- a contiguous current generation with the
  superseded generation kept alive for outstanding references -- reaches D3 and D4 by a different
  route, keeping whole buffers rather than individual slots. Whether it joins D6's substrate or
  keeps that shape is open.

## Cross-references

- [storage-owns-its-value](storage-owns-its-value.md) -- the decision this completes for two of the
  three variable-size containers.
- [jit-aggregate-realization](jit-aggregate-realization.md) -- erasure as the aggregate realization;
  what an erased container must do for its elements is constrained here, not whether it is erased.
- [value-projection-write](value-projection-write.md) -- superseded in part for the components that
  have storage identity, which includes every element named here.
- `../architecture/storage.md` -- the language contract: identity independent of position and of
  membership, and membership independent of lifetime.
- `../architecture/lifetime.md` -- the regimes a detached element's scope-bounded owner must fit.
