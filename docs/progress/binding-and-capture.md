# Binding and capture reset

Reset of the closure-capture and local-reference model to the contract in
`architecture/binding_and_capture.md`.

## Motivation

The reference model resolves a local as its original declaration plus a block-hop distance, and
resolves a closure's receiver and captures against that original site. This holds across one closure
boundary but not two: a binding captured two closure levels deep -- or a member / receiver access
inside a doubly-nested closure, such as a `fork` branch that contains a `$strobe` or a nonblocking
assignment -- reaches past an intervening closure and yields an invalid reference. The receiver has
a separate per-level mechanism that only masks the same defect for itself. The defect is the
resolver abstraction, not a receiver special case.

## Target

Per `architecture/binding_and_capture.md`:

- A reference names a binding materialized in its own callable body; a reference into another
  callable body is unrepresentable.
- A binding crosses a closure boundary only by capture, forwarded one boundary at a time (recursive
  materialization), so any nesting depth is correct by construction.
- The receiver is an ordinary captured binding; no receiver-specific capture mechanism remains.
- Capture transport separates the carrier (what crosses a boundary) from the view (snapshot, alias,
  or owning -- chosen per body by its requirement).
- Object-graph navigation across the hierarchy stays explicit runtime navigation, unchanged.

## Status

- [x] Architecture contract written and cross-referenced from the callable, MIR, and
      reference-resolution contracts.
- [ ] Implementation. This is a single landing: reference identity becomes body-local, per-body
      materialization with recursive capture forwarding replaces the block-hop resolver, the
      receiver folds into the ordinary binding model, and the block-depth reference machinery is
      removed.

## Interim

The nonblocking-assignment-into-an-enclosing-scope case (the ibex flop pattern) already lowers and
the originally-crashing modules emit; the full reset above supersedes that narrower fix.
