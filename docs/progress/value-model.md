# Value model

Tracks the migration that gives every runtime value one discipline: its SystemVerilog declared type
is an immutable descriptor fixed at construction, the value itself is an ordinary value (copy, move,
and assignment all replace the whole value), and SystemVerilog assignment semantics -- preserve the
destination's declared type -- live at the store boundary and the variable cell, not in the value's
assignment operator. Pure-refactor workstream: no user-visible behaviour change; the goal is the
architecture and the closure of a class of shape / assignment bugs.

Generalizes `../decisions/integral-representation.md` (one value type per integral; shape as runtime
state, not a compile-time type) from the integral value to the whole value layer. That
representation choice stays -- the value remains "fat". This workstream makes the declared type
immutable and lifts assignment semantics out of every value type uniformly.

## Why

The value layer carries two disciplines that should be one. Most value types -- the real family, the
string, the product and sum aggregates -- are pure values: the host type is the SystemVerilog type,
assignment replaces the whole value, and the type relocates safely inside the standard containers
with no special handling. One value type, the integral value, is shape-bearing: part of its declared
type (width, signedness, state domain, packed dimensions) is mutable runtime state. To keep a
variable's declared type across assignment, that value type's assignment preserves the destination's
shape instead of adopting the source -- and that single choice forces a chain of special cases: a
guard that the source already matches, an uninitialized sentinel that adopts a shape on first store,
a hand-written moved-from that stays valid, and a relocation-only swap that bypasses the preserving
assignment.

The discipline does not stay contained. Every aggregate or container holding an integral leaf
inherits it: a product's assignment runs the preserving assignment member-wise; a container of
integrals cannot use ordinary relocation and grows a bespoke swap; a container recovers each
element's runtime shape only by holding a prototype element. The genuinely pure-value leaves are the
real family and the string alone; almost everything in SystemVerilog bottoms out at an integral
leaf, so almost everything pays the tax.

Mixing the value and storage-cell roles in one object is the root of a bug class (a chained
multi-dimensional read once dropped its shape and read back as a single bit) and of every special
case above. The fixed-size unpacked array shows the asymmetry sharply: its extent is declared type
exactly as packed dimensions are, yet its assignment adopts the source's extent with no guard at all
-- the same latent corruption the integral value guards against, entirely unguarded. The guard lives
in one place and is missing in the symmetric place, which is the signature of a responsibility
sitting in the wrong layer.

A measurement pins the cost of the fix. The store boundary already converts width, signedness, and
state domain explicitly: when a right-hand side differs from the destination on any of those axes, a
conversion is already inserted upstream. The one thing the value's preserving assignment does that
the store boundary does not already supply is the same-width dimension reshape -- because a
same-width difference of only the dimension stack is, to the front end, the same integral type and
draws no conversion. So the cleanup is not "rewrite every store"; it is "lift one remaining
conversion axis -- dimensions -- to the store boundary, where the other three already live".

## Target architecture

- **Immutable declared type.** Every shape-bearing value holds one immutable descriptor: the
  integral value's dimension stack, signedness, and state domain (width derived once from the
  dimensions); a fixed array's extent; an aggregate's component descriptors. A rank-0 scalar is the
  empty dimension stack (width 1), distinct from a length-one vector. There is no zero-width
  pseudo-state.
- **Pure values.** Copy, move, and assignment all replace the whole value -- descriptor and contents
  together. No value type carries a sentinel, a shape-preserving assignment, a hand-written
  moved-from, or a relocation-only swap. The pure-value leaves are the shape every value type
  matches.
- **The variable cell owns the declared type.** A declared variable -- observable or not -- is
  constructed with its declared type and default. A store overwrites the cell's contents while
  keeping its declared descriptor, and requires the right-hand side to already be at the destination
  representation; the cell asserts this and performs no reshape of its own. The work the integral
  value's preserving assignment used to do moves here, once, for every value type.
- **The store boundary converts to the full declared type.** A conversion's sole meaning is "make
  this value into the destination's full declared representation", dimensions included. The
  same-width dimension reshape is one axis of that conversion, not a special case the cell smuggles
  in. Every store carries the conversion upstream so the right-hand side already matches before it
  reaches the cell.
- **Selector results are typed by lowering.** The shared selector resolution maps (source type,
  selector operands) to (offset, result shape); read and write selectors consume the same result.
  The result's signedness and state domain are decided by lowering per the LRM and handed to the
  value-producing selector, which asserts its shape agrees, instead of the runtime re-deriving them.

## Invariants

**Immutable declared type.** A constructed value has an immutable declared type. Operations may
change only its contents; they may never patch its declared type after construction. The two
sanctioned type-setting moments are constructing a new value and a cell adopting its declared type
at initialization.

**Two kinds of assignment.** Every assignment in the emitted program is exactly one of two kinds,
and the distinction is enforced, not incidental:

- A **semantic store** -- procedural, continuous, and nonblocking assignment; initialization; output
  / ref / inout writeback; port bind; system-task backdoor write; foreign-boundary write; and a
  field / element / selected-lvalue write. A semantic store requires the right-hand side to already
  match the destination's full declared type before the store; the conversion is upstream.
- An **implementation relocation** -- ordinary value movement the host language performs: container
  reallocation, in-container reordering, returning a temporary, a plain copy or move. A relocation
  never inserts a SystemVerilog conversion; it is a whole-value adopt.

After the pure-value flip the relocation kind is correct by construction; the semantic-store kind is
made correct by the converting store boundary. The two must never be confused -- a semantic store
that reaches for the bare whole-value assignment is a defect, even though that assignment exists and
compiles.

**Representation equality, named precisely.** The layer distinguishes three notions and uses each at
its own site: _same layout_ (identical dimension stack), _same representation_ (same layout plus
signedness plus state domain), and _same raw storage domain_ (compatible bit-plane layout for a
low-level copy). A typed store asserts _same representation_ -- never the weaker raw-storage check,
which would let a layout or signedness drift pass silently. No surviving check may be named for
"shape" while comparing only width.

## Done

- [x] **Materialization builds shape at construction.** Every value-producing selector constructs
      its result already carrying its derived shape; no value is built flat and then reshaped. This
      closed the original chained-read bug.
- [x] **Single selector resolution.** The read and write selectors share one offset / result-shape
      computation, so the two sides cannot drift -- the structural cause of the original bug.
- [x] **Selector signedness coincidence audited.** A select's result is unsigned per LRM 11.5.1, the
      runtime selector already produces unsigned, the only signed-result case (a signed aggregate
      field) is already re-tagged by lowering, and a select feeding an arithmetic or comparison is
      already wrapped in a conversion. So the selector-typing step (D3 below) is a cleanup that
      removes a latent coupling, not a live correctness fix, and need not precede the flip.

## Steps

The IDs are stable references; the list is in execution order.

- [x] D1 -- **Canonicalize the declared-type representation.** Every typed integral value derives
      its width / layout / signedness / state queries from one grouped descriptor, and the misnamed
      "same shape" check (which only ever compared width and state domain) is renamed to the
      storage-domain notion it actually is.
- [x] D2 -- **Total store-boundary conversion and the typed cell** (the integral value; D7
      generalizes the mechanism to every family). Every semantic store carries an upstream
      conversion to the destination's full declared type, dimensions included; the missing dimension
      axis was added to the conversion, the other three already existed. The variable cell installs
      its declared type and default at construction, then stores by overwriting contents while
      keeping its declared descriptor, asserting same-representation -- including the first semantic
      store, so a missing conversion on a declaration initializer is caught too. A mismatched
      right-hand side fails with an explicit missing-conversion message. The cell installs its type
      through an explicit construction-time step rather than adopting it from whichever store runs
      first; an empty pre-install state survives only as private plumbing in the shape-erased
      backend, never observed.
- [ ] D3 -- **Selector results typed by lowering.** The value-producing selector receives the
      lowering-decided result type and asserts its shape matches the resolved shape, instead of the
      runtime re-deriving signedness or state. Cleanup (see Done): it removes the correct-by-
      coincidence coupling rather than fixing a live bug.
- [x] D4 -- **Flip to pure value.** Assignment on every value type became ordinary whole-value
      replacement; the integral value's shape-preserving assignment, hand-written moved-from, and
      the container relocations that existed only to dodge the preserving assignment were all
      removed; aggregates and containers fall to pure member-wise replacement.
- [x] D6 -- **One packed-type descriptor.** The integral value's declared type, every construction
      entry's shape argument, and the base type an enum declares are one descriptor (dimension
      stack, signedness, state domain; width derived). The earlier fork -- a flat width-only
      descriptor for enum bases versus a loose dimension-list passed as a separate construction
      argument -- is gone, so no construction takes a raw shape list; generated code names the one
      descriptor. This is the packed-type vocabulary reconciliation D1 deferred.
- [x] D7 -- **Generalize the typed-store discipline to every value family.** The construction-time
      type install and the store-boundary conversion, first realized for the integral value alone,
      now cover every observable value cell and every value family: each declared cell installs its
      representation at construction, and every semantic store converts its right-hand side to the
      destination's declared representation regardless of family, including the local-declaration
      stores that previously bypassed the conversion. The queue -- the one container that still kept
      its declared element shape and bound through a value-side preserving assignment -- became an
      ordinary value: its element shape and bound are carried by its construction, so an empty
      concatenation matches its declared type, and a differently-bounded source is conformed to the
      destination's bound at the store boundary, so no sentinel and no shape-keeping assignment
      remain. This removed the last place the discipline was forked on whether a value was the
      integral one.
- [ ] D5 -- **Cleanup and descriptor sharing, profile-driven.** Whether to back the immutable
      descriptor with a shared interned handle is decided after measuring copy cost on the
      temporary-heavy value paths. Done when the cost is measured and the share / intern decision is
      recorded.

## Out of scope

- The container out-of-bounds default mechanism (the per-container prototype element) stays as it
  is; revisiting it touches `../decisions/runtime-shape-and-default-value.md` and is a separate
  review. This workstream removes only the container workarounds that exist purely to dodge the
  integral value's preserving assignment -- never a guard that maintains out-of-bounds, default, or
  collection semantics; the two are told apart by a targeted audit before any removal. The
  distinction between a fixed array's extent (declared type, an exact-type store) and a dynamic
  array's or queue's length (value state, a whole-value adopt) is honoured: only the former is a
  representation a semantic store must match.

## Cross-references

- Integral representation choice this generalizes: `../decisions/integral-representation.md`.
- Container default mechanism left in place: `../decisions/runtime-shape-and-default-value.md`.
- Selector read / write surface: `operators.md`.
