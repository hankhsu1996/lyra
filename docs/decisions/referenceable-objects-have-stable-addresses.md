# A referenceable class object has a stable address

Date: 2026-09-11 Status: accepted

## Context

A class property is one of the four things LRM 13.5.2 allows as a `ref` actual, and
[reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) makes a reference one machine word
pointing at storage. So `foo(c.x)` hands the callee a pointer into the middle of a managed object,
and that pointer has to stay correct for as long as the callee holds it.

Two different things can go wrong with it, and they are worth separating because one of them is
obvious and the other is the reason this entry is longer than the question looked:

- **The object moves.** A relocating collector invalidates the pointer.
- **The object is collected.** The last handle is dropped while the callee still holds the
  reference, and the pointer is left naming freed storage.

Non-moving storage answers the first and says nothing about the second. And the second is not
settled by the standard: Table 8-1 p.182 says unreferenced objects are garbage collected, 8.4 never
defines what counts as a reference, and nothing says whether an outstanding `ref` to one property is
one. The evidence is an asymmetry rather than a silence -- 13.5.2 explicitly extends the lifetime of
a _container element_ passed by reference and says nothing about a class property, so the standard
has the vocabulary for lifetime-extension-by-reference and applied it to one category only.

## Decision

**D1. A class object that a reference can point into does not move.** The collector does not
relocate objects. This is a property the reference representation depends on, not a collector
implementation detail.

**D2. A reference to a property is an interior pointer.** No handle, no indirection, no
collector-aware slot -- the same one-word reference every other referent gets.

**D3. An object with an outstanding interior reference goes on existing, and that is a property of
the object rather than retention by the reference.** This is the exact analogue of LRM 13.5.2's
detached container element, which "shall continue to exist within the scope of the called
subroutines until they complete" -- a property of the element, not of the reference that kept it.
`storage.md`'s invariant that a reference aliases and never owns is therefore untouched: what
survives is decided about the object, at the bind, for a bound duration.

**D4. This is Lyra policy where the standard is silent.** Whether a `ref` to a property counts as a
reference for reclamation is not stated anywhere. Lyra picks that it does, because the alternative
is a dangling interior pointer and a silent wrong answer, while this costs at most an object that
nothing else names, for the length of one call.

**D5. A future moving collector pins; it does not change the reference.** If fragmentation or
throughput later argue for relocation, an object with an outstanding interior reference is pinned
for that reference's lifetime. The reference representation is not reopened for it.

## Invariants

1. A reference into an object is valid for its whole lifetime: the object neither moves nor is
   reclaimed while it exists.

2. What keeps the object alive is a fact recorded about the object, bounded by the referring
   subroutine's scope. Nothing about the reference's representation changes to express it.

3. A collector change may not require a reference change. Relocation is admitted only with a
   mechanism that leaves the one-word interior pointer correct.

## Rejected

- **A moving collector with an indirect property reference.** The third of the three routes: a
  reference names a collector-aware slot the collector updates when it moves the object. It is the
  only one of the three that keeps full collector freedom, and it is rejected because it pays for
  that freedom on the wrong path -- a dependent load on every access through every property
  reference, permanently, to preserve an option nothing has yet asked for.
  [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) measured what that load costs
  and rejected the same shape for the same reason.

- **A moving collector with pinning, now.** All of the machinery of D5 with none of the benefit: the
  pin bookkeeping, the unpin-on-scope-exit path, and a collector that must handle pinned regions,
  bought before any measurement says relocation is needed. D5 keeps it available; adopting it now
  would be paying its complexity in advance.

- **Letting a property reference dangle when the last handle drops.** The reading where `ref` does
  not count as a reference for reclamation. It is not excluded by the text, and it is rejected
  because the failure mode is a silent wrong answer -- a read through a pointer into reclaimed
  storage -- where the other reading's cost is an object surviving one call longer than it had to.
  The standard's own instinct in the analogous case is the same: it extends the container element's
  lifetime rather than letting the reference dangle.

- **Making a property reference a managed reference.** It would keep the object alive by the
  existing mechanism with no new concept. Rejected: a managed reference is an owning edge the
  collector traces, and a `ref` is an alias with a scope-bounded life; making one the other would
  give a `ref` the copy-and-outlive semantics of a handle, which is not what the language says a
  `ref` is.

## Consequences

- **Non-moving stops being only a realization choice.** `../architecture/lifetime.md` records the
  initial collector as precise, stop-the-world, non-moving, single-threaded mark-sweep and calls
  that a realization choice. The non-moving part is now load-bearing: a relocating collector owes
  D5's pinning before it may be adopted.

- **The collector needs to know an interior reference exists.** D3 is a requirement on the
  collector, and the mechanism is the collector's own -- a pin count on the object, or the referring
  frame enumerating interior references at a safepoint. `lifetime.md` already requires every value
  that survives a safepoint to live in traceable, Lyra-owned storage, so the information is
  reachable; what is not decided here is which of the two shapes carries it.

- **Property storage may be inline in the object.** With the object's address fixed, a property's
  address is fixed by its offset, so nothing about identity forces properties into separately
  allocated slots. Whether they should be is a separate question, and it is coupled to the value
  representation rather than to this entry.

- **The same argument covers a structural object.** A module or scope instance is not collected at
  all, so a reference into one has neither problem; this entry is needed only where reclamation is
  automatic.

## Cross-references

- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- the one-word reference this
  keeps valid, and the conflict with the collector contract that this resolves.
- [object-model](object-model.md) -- the managed object this constrains the storage of.
- [managed-value-realization](managed-value-realization.md) -- what a managed value may not live in,
  which this does not change.
- `../architecture/lifetime.md` -- the managed regime, the collector's latitude, and the safepoint
  contract the mechanism must fit.
- `../architecture/storage.md` -- a reference aliases and does not own, which D3 is framed to
  respect.
