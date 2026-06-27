# Packed value model

Tracks the migration that separates the roles the runtime integral value conflates today into clean
layers, so an integral value is an ordinary value and SystemVerilog assignment semantics live in the
variable cell. This is a pure-refactor workstream: no user-visible behaviour changes; the goal is
the architecture, and the closure of a class of shape/assignment bugs.

Extends the representation choice in `../decisions/integral-representation.md` (one value type for
every integral, three attributes plus a dim stack); that decision stays, this workstream makes the
descriptor immutable and lifts the cell semantics out of the value.

## Why

The runtime integral value is simultaneously three things:

- a **value** -- the result of arithmetic and comparison;
- a **typed storage cell** -- assignment preserves the variable's declared type, and a zero-width
  sentinel adopts a shape on first store;
- the **carrier of dimensional shape** for chained selects.

Mixing these is the root of a bug class (a chained multi-dimensional read once dropped its shape and
read back as a single bit) and of the special cases that surround it: shape-preserving assignment,
the zero-width sentinel, and the relocation-only swap exemption. Each is a symptom of one object
owning three responsibilities.

## Target architecture

- **Immutable shape descriptor** -- a dimension stack plus its total width, the width derived once
  at construction and never independently settable. A rank-0 scalar is the empty dim stack (width
  1), distinct from a length-one vector.
- **Immutable value type** -- the shape plus signedness plus 2-state / 4-state domain.
- **The integral value** -- a value type plus its bits, with ordinary value semantics (copy, move,
  and assignment all replace the whole value) and no storage sentinel.
- **The variable cell** -- owns its declared value type, is constructed with it, and stores an
  already-converted right-hand side by overwriting bits while keeping its declared descriptor. The
  assignment conversion is inserted upstream during lowering; the cell requires the right-hand side
  to already match and never converts.
- **Selector resolution** -- one shared computation maps (source shape, selector operands) to
  (offset, result shape); read and write selectors consume the same result. The result's signedness
  and state domain are decided by lowering per the LRM, not re-derived at runtime; the
  value-producing selector receives that type and asserts its shape agrees.

## Invariant

A constructed integral value has an immutable descriptor (shape, signedness, state domain).
Operations may mutate only its bits; they may never patch its shape, width, signedness, or state
after construction. The two sanctioned descriptor-setting moments are constructing a new value and a
cell adopting its declared type at initialization.

## Done

- [x] **Materialization builds shape at construction.** Every value-producing selector constructs
      its result already carrying its derived shape; no value is built flat and then reshaped. This
      is the first half of the invariant above and closed the original chained-read bug.
- [x] **Single selector resolution.** The read and write selectors share one offset / result-shape
      computation, so the two sides can no longer drift -- the structural cause of the original bug.

## Steps

The IDs are stable references.

- [ ] V1 -- **Immutable shape and value descriptor.** Introduce the immutable shape (dims plus
      cached width, factory-only construction, rank-0 scalar = empty dims, no zero-width
      pseudo-state) and value type (shape plus signedness plus state). The value holds one
      descriptor; every width / shape / signedness / state query derives from it. Replace the single
      ambiguous "same shape" check (which today compares only width and state, not the dims) with
      the three notions the layer actually needs: same layout (shape), same representation (shape +
      signedness + state), and same storage width / domain (the raw bit-copy compatibility check).
      Done when the value holds one immutable descriptor and the three equality notions are distinct
      and used at the right sites.
- [ ] V2 -- **Integral value becomes a pure value.** Remove the zero-width sentinel and the
      shape-preserving assignment from the value; copy / move / assignment / swap all become
      ordinary whole-value semantics. Done when the value has no special-case assignment and no
      sentinel state.
- [ ] V3 -- **Typed cell store.** The variable cell owns its declared value type, is constructed
      with it and its default, and stores a right-hand side by overwriting bits while keeping its
      declared descriptor; it requires the right-hand side to already be at the destination
      representation. The sentinel's former job (first-store shape adoption) moves here. Done when
      first-store adoption no longer exists on the value and the cell holds the declared type.
- [ ] V4 -- **Store-boundary invariant.** Every lvalue write -- procedural assignment, cell store,
      port bind, initialization, system-task backdoor write, foreign boundary -- carries an explicit
      upstream conversion, so the right-hand side already matches the destination representation,
      shape included. A store-time compatibility check fails with an explicit "missing upstream
      conversion" message rather than silently converting. A static verifier over the mid-level IR
      is the eventual enforcement; the runtime check is the first guard. Done when stores assert
      representation compatibility and conversion targets carry full type including shape.
- [ ] V5 -- **Selector materialization receives result type.** The shared resolver returns only
      offset and result shape; the value-producing selector receives the lowering-decided result
      type (signedness / state) and asserts its shape matches the resolved shape, instead of
      transiently inheriting the source's signedness. Done when no selector result infers its own
      signedness or state.
- [ ] V6 -- **Descriptor sharing, profile-driven.** The descriptor is immutable and may be shared;
      whether to back it with a stable interned handle is decided after measuring copy cost on the
      temporary-heavy value paths (small values dominate). Done when the cost is measured and the
      share / intern decision is recorded.

## Cross-references

- Selector read / write surface: `operators.md` (W4..W6).
- Integral representation choice this extends: `../decisions/integral-representation.md`.
