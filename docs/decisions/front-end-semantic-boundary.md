# The front end owns semantic resolution, and Lyra translates what it resolved

Status: Accepted, 2026-07-08.

## Requirement

What a name in the source denotes is answered once, by the component that resolved it, and every
consumer -- a read, a write, a change observed -- reaches the same answer. Two defects raised this.
A combinational reader of a signal in a sibling generate block lost its subscription, or kept it,
depending on declaration order, because Lyra looked the target up in a table of its own unit's
declarations and dropped what was not on the reader's enclosing chain. And an `always_comb` whose
dependency was read inside a called function was never woken (LRM 9.2.2.2.1). Both rebuilt, from
less information, a fact the front end had already resolved.

## Choice

**slang owns semantic resolution and the extraction of sensitivity; Lyra owns translating what it
resolved.** slang is the authority for which declaration a reference denotes, the route a
hierarchical name takes, which dependencies form a procedure's sensitivity, and whether a target is
a net, a variable, a port or a `ref`. Lyra re-resolves no name, re-derives no up, down or root
classification, and classifies no reference from degraded information. A fact the front end has
resolved is translated once and consulted by every consumer in every direction, never recomputed.

The route from a reader to a target is a structural relation between two positions the front end has
already placed in its elaborated hierarchy, so the translation needs no name resolution.

## Alternatives turned down

- **Deciding how to reach a target, or whether to keep a dependency, from the target's declaration,
  a table of the unit's declarations and a walk up the reader's enclosing scopes.** It fails one
  answer per name. The failure rests on that table recording only the unit's own declarations, so a
  failed walk means only that the target is not an enclosing scope, which the consumer then read as
  "drop" or "illegal".
- **One read set for every consumer of inferred sensitivity.** It fails the standard's per-construct
  rules, since an `always_comb` must see what the functions it calls read, while `@*` stops at a
  call's arguments (LRM 9.2.2.2.1, 9.4.2.2). The front end defines the surface for each.
- **A walk of an assignment's target, ahead of lowering, asking the declaration table about each
  name.** It fails the front end's verdict on legality and the reference's own answer on reach. It
  refused a continuous assignment to a hierarchical target that LRM 10.3 makes ordinary.

## Consequences across layers

- Read, write and observation of one target reach it through one translated route and one bound
  endpoint ([`reference_resolution.md`](../architecture/reference_resolution.md)), and which surface
  each consumer of sensitivity reads is [`scheduling.md`](../architecture/scheduling.md)'s.
- Which text is legal is the front end's judgement too
  ([the-standard-decides-what-text-is-legal](the-standard-decides-what-text-is-legal.md)), and which
  instances elaborate alike
  ([every-body-the-front-end-does-not-share-is-lowered](every-body-the-front-end-does-not-share-is-lowered.md)).
- The choice is held by a check rather than by this sentence. A018 of
  `tools/policy/check_architecture.py` refuses a construct consulting the unit's declaration
  registry outside its owners, since a gap between two axes each covered by its own tests passes
  every suite.
