# Hierarchical Callable Dispatch

## Date

2026-09-09

## Status

Accepted. Extends `hierarchical-reference-routing.md`, whose D5 reserved hierarchical callable
dispatch as a target family and stated the five things an entry adding one owes.

## Why this decision matters

A hierarchical name may end at a subroutine as readily as at a declaration that holds a value. LRM
23.8's Syntax 23-8 lists a function and a task beside a variable, a net, a parameter, a port and a
named block; LRM 23.6 lists "reference subroutine names" beside read, write and trigger as one of
the four things a name does with what it reaches.

Lyra carried the value half of that list over one route and the callable half over no route at all:
a reference to a declaration was a climb, a descent and a leaf, while a reference to a subroutine
was a climb and nothing else. Measured across LRM 23.8's own list crossed with every route form -- a
child instance, an instance-array element, an enclosing module, a sibling of an ancestor, `$root`, a
generate block, a named block, a static task's body, an interface instance, an interface port, a
package -- **every failure in that matrix was a callable or a cancellation target**. A climb alone
cannot say "out one level and back down into a sibling", so a callable that is not on the reader's
own enclosing chain had no expressible reference at all.

That is the forbidden "a reference shape per direction or per lexical form", reached not by adding a
shape but by one shape never having grown a route.

## The model

**A call to a subroutine another scope declares is the route to that scope, plus whatever answers
the name at the end of it.** The route is the one a reference to a declaration in that scope takes,
built by the same walk and classified per segment by the same rule; the promise the declaring unit
made decides only what the name resolves against.

## Decisions

### D1. A callable reference is a route, not a hop count

A call to a subroutine another scope declares carries the same walk a value reference carries: where
navigation starts, the descent from there, and what it ends at. A subroutine the reader's own scope
declares is the zero-length walk -- the degenerate case, as a direct member is for a value -- and
stays a direct call; it is not a special form, it is the empty route.

The hop-only shape is deleted rather than kept beside the routed one. It is not a fast path: a
zero-hop route is already the fast path, and keeping both would restore the per-form species the
routing decision forbids.

### D2. What the route ends at is classified by what the referrer can compile against

The same question the value side answers -- does the referrer have a declaration to compile against
for this target -- and the same three answers:

| When                                                 | What ends the route                          |
| ---------------------------------------------------- | -------------------------------------------- |
| this artifact owns the declaring scope's class       | that scope's own identity for the subroutine |
| the declaring unit put the callable on its signature | the position its signature gave it           |
| nothing was published                                | the name, which the scope answers with       |

Only the third is a target no existing endpoint expresses, so only it adds a leaf: the first two end
at something already reachable -- a callable this artifact declares, and an object whose unit
promised the name -- and each was already a reference of its own. What the added leaf holds is an
entry, which is why it is a leaf at all: the answer has to seal with the route rather than be looked
up per call.

No arm is invented for this family. A package or `$unit` subroutine keeps its own by-name form,
because a namespace unit has no instance and so no route to take -- the same reason a package
variable is not a routed reference either.

### D3. A module's subroutine is opaque in both directions, and publishing it is forbidden

A module's signature is its parameters and ports. A subroutine of one was promised to nobody, so a
name reaching it takes the opaque arm -- the same arm the same module's internal variable already
takes, which is why reading `c.count` works today and calling `c.tick()` does not.

**Promising it instead is not a smaller version of this decision; it is excluded.** An upward enable
(`Top.bump()` from inside a child) would make the child depend on its parent while the parent
already depends on the child, and the declared-dependency graph between units must stay acyclic --
`compilation_unit_model.md` terminates the readable closure on exactly that. A surface that admits
the downward direction and not the upward one would then be a reference shape per direction, which
is the shape being removed. So both directions are opaque, and the asymmetry against an interface
stands: an interface publishes its whole declared surface, so its subroutine is the published arm,
and that arm is untouched by this entry.

### D4. A scope answers a name with an entry, the way it answers one with an address

Realizing the opaque arm needs one new runtime capability and no new route mechanism. A scope
already registers its own declarations under their source names during construction and answers
by-name queries about them; what it cannot answer with is a callable. It gains a second registry of
the same shape, keyed the same way, answered at the same phase.

The precedent is the DPI-C export table (LRM 35.4), which is already a named callable a scope
publishes as a function pointer with its prototype erased and restored at a call site generated from
the same declaration. A hierarchical enable is that with the name resolved through the object tree
rather than program-globally. The erasure is safe for the same reason: the entry and the call site
are generated from one front-end declaration and cannot disagree.

**The entry's shape is decided by time, not by convenience.** A hierarchically enabled task may
consume time, so the caller's process suspends inside another unit's body; the entry is therefore
the same suspendable shape a subroutine of this unit is already emitted with, and the phase-driving
entries a scope carries -- plain native functions over the receiver -- are not the shape to reuse.

### D5. A cancellation target is another leaf, not a second mechanism

`disable` naming a block or task elsewhere on the hierarchy (LRM 9.6.2) reaches the same route to a
scope and ends at that scope's cancellation target. It gains a leaf beside the callable's and shares
everything else. What a `disable` invalidates and what leaving a target does are settled by
`disable-scope-invalidation.md` and do not move.

This is what makes the statement's reach one question rather than two: what a `disable` can name is
then decided by the route to the target, the way what a read can name already is, rather than by
which side of the reader's own declaration chain the target happens to sit on.

## What D5 of the routing decision asked for

1. **A target whose access surface no existing protocol expresses.** A callable. Every existing leaf
   ends at storage or at an object; an entry is neither, and no sequence of the existing protocols
   produces one.
2. **The access protocol, with sealed-endpoint semantics.** A call needs the object and the entry,
   and each is a route ending where it ends, so the reference seals two endpoints over one walk
   rather than inventing a slot that holds a pair. A function call yields a value; a task enable
   suspends the caller until the body completes, exactly as an intra-unit enable does. The
   sensitivity surface is empty -- a callable is not observable, so nothing subscribes to one, and
   this family adds nothing to how a run is scheduled.
3. **Resolution and sealing.** The route's segments classify by the existing rule and execute in
   Resolve with every other route. Sealing validates that the name was answered; a name a scope does
   not answer is a user-diagnosable elaboration failure at the sealing barrier, never a runtime
   fallback and never a hot-path lookup.
4. **The MIR representation.** The receiver is the borrowed object pointer a route ending at a scope
   already produces. The entry is a callee, and the callee vocabulary already carries an arm for a
   callable this unit does not declare.
5. **A hot-path realization per backend.** The realization is to restore the erased entry to the
   prototype the call site compiled against and invoke it on the sealed receiver, walking nothing at
   call time. The C++ backend does that against the declaring unit's emitted declaration. The
   execution backend has no call through a computed code address yet -- the same thing that keeps a
   DPI-C export off it -- so it refuses the arm where it is declared, with its own reason.

## Forbidden shapes

- A callable reference carrying a climb without a descent. A climb alone can only say "out", so it
  cannot reach a callable that is not on the reader's own enclosing chain.
- A module publishing its subroutines, in either direction. See D3.
- A typed call naming a subroutine the declaring unit did not publish -- the artifact-boundary
  violation, identical in kind to a typed segment onto an unpublished member.
- A second by-name mechanism for callables beside the one a scope already uses for its declarations.
  One registry shape, one resolution phase.
- A per-access lookup of the entry on the simulation path. The entry seals with the route.
- A `disable` reach decided anywhere other than by the route, or decided in two places.

## Consequences

- The hop-only callable reference is deleted; every call to a subroutine outside the reader's own
  scope goes through the route.
- A subroutine declared in a generate block is reachable from the enclosing module's body, from a
  sibling generate block, from a loop-generate iteration and through an absolute path -- all
  intra-unit and all typed, because the route to the block is the one a value read already takes.
- A subroutine and a `disable` target reached by a hierarchical name are not separate gaps: one
  route serves both, so neither can be finished without the other being one leaf away.
- Every scope publishes every subroutine it declares, because a unit compiled alone cannot know
  which of them a name will reach. The cost is one static record and one entry per subroutine, in
  every design, whether or not anything names one.
- The execution backend cannot borrow the target language's name resolution the way the C++ backend
  can, so what it owes is a call through a computed code address; until it has one it refuses this
  arm, and the same thing keeps a DPI-C export off it.

## Cross-references

- `hierarchical-reference-routing.md` -- D5 reserved this family and set the five requirements
  above.
- `../architecture/reference_resolution.md` -- per-segment classification, and the forbidden
  per-direction reference shape.
- `../architecture/compilation_unit_model.md` -- what each unit kind publishes, which is what makes
  a module's subroutine opaque and an interface's published.
- `disable-scope-invalidation.md` -- what a `disable` invalidates, which this entry does not move.
