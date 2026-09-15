# One event source concept, two physical realizations, chosen by measured density

Date: 2026-09-11 Status: accepted

## Context

[event-subscription-model](event-subscription-model.md) D5 made the dependency an event source
rather than a declared variable, and [object-is-an-event-source](object-is-an-event-source.md)
defined the second kind. Both entries describe what a source _is_ and neither says what one costs to
have. The question left standing was whether a source should exist for every entity that could carry
one, or be created when something first subscribes.

That was argued from the premise that most variables have no waiter. The premise was counted rather
than assumed, on the Ibex Simple System design and on the Ibex verification corpus, and it holds for
one kind of source and fails for the other.

**A declared variable is subscribed to far more often than not.** Of 2109 instantiated cells, 83.1
percent are waited on by something -- an explicit `@`, an implicit sensitivity, a continuous
assignment's read set, or the implied continuous assignment of a port connection. Only 12.3 percent
are neither subscribed to nor externally reachable. Fanout is short: 77.6 percent of subscribed
cells carry exactly one subscription, 94.3 percent carry three or fewer, and the design maximum
is 39.

**A class object is almost never subscribed to.** In 22531 lines of verification code across 175
class declarations, every one of the 65 event controls written inside a class dereferences a handle,
and only five property names are ever the root of such a path -- all long-lived structural handles
set during construction. The objects that dominate allocation, sequence items and transactions,
never appear in an event expression at all.

The two established answers to "waiters are rare" both put nothing in the object. Go's runtime keeps
no waiter list per address: `semtable` is a hash table of `semaRoot`s keyed by the address, and a
waiting goroutine's `sudog` is threaded there, so an uncontended address costs zero bytes. HotSpot
overloads one word of the object header, and only inflates it into a real `ObjectMonitor` -- taken
from a thread-local free list -- when a second thread actually contends. Neither design would be
chosen for a population that is 83 percent subscribed, and both are exactly right for one that is
almost none.

## Decision

**`EventSource` is one concept with two physical realizations, and which one an entity gets follows
from the measured density of its population, not from a wish for uniformity.**

**D1. The semantic abstraction stays single.** A leaf depends on event sources, a write publishes to
one, and nothing above the realization layer distinguishes the kinds. Every rule in
`event-subscription-model` and `object-is-an-event-source` is stated over that one concept and none
of them is qualified by this entry.

**D2. A declared variable's source is provisioned, not discovered.** Its population is fixed by
elaboration, most of it is genuinely subscribed to, and the write path is the hottest in the
simulator. Nothing on that path may consult a table keyed by an address, and the source is not made
conditional in order to save a minority of the population. Where the source physically sits is not
settled by this: an emitted pointer or index into a compact table is a compile-time fact and
satisfies it, and nothing here asks the source to live inside the variable's own storage.

**D3. A class object's source is materialized on demand.** Its population is unbounded and driven by
allocation, and the subscribed part of it is a handful of structural objects. An ordinary object
carries no observation state and pays nothing for the possibility of being waited on.

**D4. A hash table keyed by the storage address is refused for the declared variable.** It would be
paid on 83 percent of writes to buy the 17 percent that need nothing. This is the specific shape the
measurement rules out, and it is recorded because it is the one an intuition about sparsity leads
to.

**D5. The physical shape of the declared-variable source is not decided here, and the size of it is
where the remaining gain is.** The current realization spends 48 bytes on every cell while the
median declared signal is one bit wide. Because most cells genuinely need a source, making the
source conditional recovers a sixth of that; making the source itself smaller recovers a multiple of
it. The open question is therefore the minimum shape, not the allocation policy.

## Invariants

1. A realization difference may not reach the semantic layer. Nothing that reasons about
   dependencies, publishing or filtering may ask which kind of source it holds.

2. Reaching a declared variable's source is a compile-time fact, never a run-time search. What this
   forbids is a structure consulted by the storage's address; what it permits includes a pointer or
   an index the compiler emits, and it says nothing about whether the source sits inside the
   variable's storage or beside it.

3. An object that nothing has subscribed to carries no observation state.

## Rejected

- **One physical representation for both kinds.** The uniformity is worth having and it is worth
  less than being wrong by a factor of the transaction count on one side or by a hot-path lookup on
  the other. The abstraction is what has to be single; the realization is what has to be right.

- **A lazily created source for declared variables.** The intuition this entry started from. It
  saves 17 percent of the metadata, costs a pointer on every cell to find it by, and puts a branch
  on the write path that is taken 83 percent of the time -- a test that almost never skips the work
  it guards.

- **Intrinsic observation state on every class object.** The mirror error. It would size the
  observation machinery by the allocation rate of objects that are never waited on, which is the one
  population in the language with no bound.

- **Deciding the declared-variable shape here.** It is a separate question with its own evidence,
  and answering it inside an allocation-policy entry would settle by assumption the thing the
  measurement says matters most.

## Consequences

- **The two realizations are written and tested separately**, and the seam between them is the
  publish operation, which must compile to different code for the two without either kind appearing
  in a rule above it.

- **The remaining work on the declared-variable side is size, not policy.** Of the 48 bytes a cell
  spends today, the part that a list head structurally needs is a small fraction; the rest is a
  consequence of reusing the subscription node as the head of the list of subscriptions.

- **Compile-time elimination is worth having and is not the main prize.** With whole-design
  visibility it removes the machinery from about one sixth of cells. It is also fragile in a way
  worth writing down: a feature that observes everything, such as waveform dumping, takes the
  eliminable fraction to zero for any run that uses it.

- **The object side needs the materialization point named.** Nothing here says where a source is
  created or what finds it; it says only that an unsubscribed object holds none.

## Cross-references

- [object-is-an-event-source](object-is-an-event-source.md) -- the second kind of source, whose
  population this entry measures.
- [event-subscription-model](event-subscription-model.md) -- the leaf that holds a dependency on a
  source.
- [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) -- that
  observation is a mechanism beside storage, which is what lets the two realizations differ at all.
- `../architecture/storage.md` -- storage, update and event identity as three things.
