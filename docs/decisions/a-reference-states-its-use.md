# A reference states its use

Date: 2026-09-24 Status: accepted

## Context

A name in a structural scope reaches something elsewhere in the design hierarchy, and the source's
position decides what it is used for: a hierarchical name is read, written, waited on, or names a
subroutine (LRM 23.6), and a `disable` names a block or task (LRM 9.6.2). A call made on another
unit's instance reaches that instance as the object the call is made on.

The intermediate form had one reference for all four uses, and one set of ends for its route: eight
alternatives spanning every use and every way of naming an end -- a declaration of this unit, a
position another unit's signature gave it, a name the runtime answers. Every consumer serves one
use, so each one named the ends of the other three only to refuse them: reading a value refused six
of the eight. And the only reader that wanted all eight was a classifier turning an end back into
its use, which the producer had known when it made the reference.

## Decision

**Each use is a reference of its own, and its route can end only at what that use reaches.** The
path -- how many scopes out, then which scopes down -- is one shape for every use; the end is not.

- A value read, written or waited on ends at data: a data object this unit declares, a static of one
  of a scope's bodies, a member another unit published, or a name the runtime answers.
- The object a call is made on ends at the object the steps land on.
- A subroutine reached by name ends at the entry the scope it reached answers with.
- A `disable` ends at a block this unit lays out, or at what the scope reached answers for.

A scope holds one table of routes per use, and a port connection holds its routes typed the same
way: a `ref` port's binding and an interface port's member end at data, and an interface port's
peers end at objects.

A data object and a static differ only in which table names the field they end at; they are two ways
of naming data, not two uses. A block a `disable` names is a different use, with ends of its own.

The field divides the same way. slang keeps one record of a hierarchical path and holds it in a node
per use, each with its own kind of end: a value expression ends at a value symbol, a call's lookup
at a subroutine symbol, a `disable` statement's target at the block's symbol -- decided when the
name is bound, from where it was written.

## Rejected alternatives

- **One reference, with the use recoverable from where the route ends.** The shape this replaces. A
  consumer of one use had to refuse the others' ends, and the use -- known at the producer -- was
  re-derived at each consumer.
- **One table of routes, with each use a distinct id type into it.** Keeps one table, but a value's
  id could still name a route ending at an object; the use would be stated by the id and
  contradicted by nothing.
- **Grouping the ends by how they are named first** (this unit's declaration, a signature, the
  runtime) and by use second. Every consumer serves one use, and none serves one way of naming
  across uses, so the axis that no consumer reads would be the outer one.

## Consequences

- Reading a value never meets an object, an entry or a disable target. The one refusal left is the
  one no type can state: a route of parent edges never leaves the unit, so it never ends at data
  another unit declares.
- What a route of each use is reached by -- a pointer to the cell or object, an entry's code
  address, a pointer to a disable target -- is decided by the use, and for data and objects by where
  the route ends.
- The path stays one shape and one walk
  ([hierarchical-reference-routing](hierarchical-reference-routing.md)); only the end is per use.
