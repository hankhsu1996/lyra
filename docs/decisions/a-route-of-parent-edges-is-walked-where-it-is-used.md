# A route of parent edges is walked where it is used

Date: 2026-09-24, revised 2026-10-23: a parent edge is a member of the scope it leaves. Status:
accepted.

## Context

A generate construct decides, while the tree is being built, which scopes exist and how many. What
it decides with -- a conditional's condition, a case selector and its labels, a loop's header, a
constant a block settles -- are constant expressions (LRM 27.4, 27.5), and a constant expression
nested inside a loop generate's block may name the block's index, an implicit localparam usable
anywhere in the block, or a constant the block settles from it.

A name reaching a declaration of an enclosing scope was a routed reference whose reader held a slot,
and every slot was filled in the resolve phase, once the whole tree exists
([hierarchical-reference-routing](hierarchical-reference-routing.md)). A conditional two levels
below a loop therefore read a slot nothing had filled, and the design crashed while its hierarchy
was being built -- on both backends, and on the integration design, whose control-register block
chooses a counter's implementation by `Cnt == 10` two conditionals below its loop.

## Decision

**A reference is one shape wherever its target stands: a route -- how many scopes out, then which
scopes down -- and what it ends at.** A declaration of the reader's own scope is the route that goes
nowhere, and one of an enclosing scope is a route of parent edges only (LRM 23.9); neither is a form
of its own. What the route may end at is decided by the use the name is put to
([a-reference-states-its-use](a-reference-states-its-use.md)). How the route is realized follows
from its segments:

- **Parent edges** always exist once the reader does, because every scope enclosing it was built
  first. A route made only of them has nothing to resolve and nothing to validate, so it is walked
  where it is used -- in a process body, a continuous assignment, and a generate construct's own
  condition alike. That is what makes a construction-time read of an enclosing constant work: the
  constant was settled when its scope was built, and the walk reaches it.
- **A descent, or a crossing into another unit**, may reach a scope not built yet, an alternative
  not selected, an element that does not exist, or a name only the runtime answers (LRM 23.6, 27.5).
  Such a route is walked once the tree is whole, validated there, and what it reached is kept in a
  slot of the reader's class.

A constant expression can reach only the first kind, because its operands are numbers, strings,
parameters and constant calls (LRM 11.2.1) -- never a variable, a net or a hierarchical name -- and
a function declared in a generate block is not a constant function (LRM 13.4.3). But the line is not
drawn around constants or around when a read happens: it is drawn around what a segment can fail to
reach.

The field draws it the same way. slang records a hierarchical name as a resolved path plus an upward
count, from one lookup whatever the context. An inner class in Java holds its enclosing instance in
a field its constructor sets, and a name in the enclosing instance compiles to a chain of loads
through those fields at each access, with any caching left to the layer below. A generate block here
is an object its enclosing object builds, which is the inner-class condition exactly.

Measured, on the scheduling benchmarks where sixty-four processes read and write through their
enclosing scopes: walking the parent edges ran at 40,181 cycles/s one scope out and 42,101 three
scopes out, against 22,599 and 38,899 reading a stored slot. One scope out, the two are the same
number of loads, so the first pair says more about the machine than about the shape; the three-deep
pair says the walk costs nothing measurable.

## Rejected alternatives

- **A reference form of its own for names read during elaboration**, marked by the positions the
  language evaluates then. Built and correct; rejected because it carried exactly the facts the
  routed reference beside it carries -- how many scopes out, and which declaration -- so one meaning
  had two names, and the positions evaluated during elaboration had to be listed one by one.
- **Keep every route slotted, and fill the slot of a route of parent edges while the reader is
  constructed.** Built and correct; rejected because what the slot keeps is a walk anyone can repeat
  from `self`, so keeping it is caching -- an optimization, which belongs below the layer that
  states the access, and which the measurement above says is not owed -- and because choosing the
  phase needed a predicate over the route's shape at every slot.
- **Keep one reference, and have the constructor walk its route while every other body reads the
  slot.** Built and correct; rejected because one node then lowered two ways depending on which
  callable it was lowered in.
- **Hand the constant down as a construction value of every scope between the declaring one and the
  reader.** Built and correct; rejected for a copy per scope, a registry of the scopes still being
  built, and a second way of reaching a name beside the one that already reached it.
- **Make a unit's whole generate tree one construction procedure, with genvars and block constants
  as its locals** -- how SystemC and Chisel structure a module's elaboration. No requirement of this
  subject selects it, and the per-block construction it replaces serves one: a loop's blocks compile
  to one class built with its index
  ([one-body-built-at-every-index](one-body-built-at-every-index.md)).

## Consequences

- A member of the reader's own scope and one of an enclosing scope are the same reference, differing
  only in how many parent edges its route counts.
- A route of parent edges takes no slot and no fill; a class static property reached from inside an
  enclosing scope, which already walked its parent edges at every access, is now the same rule
  rather than an exception to it.
- Neither the intermediate form nor the reference states a phase. Whether a route is walked where it
  is used or kept in a slot is decided where the reader's class is laid out, from the route's own
  segments.
- A parent edge is the inner-class field itself: a block of a generate construct is handed the scope
  building it as that scope's class, keeps it in a member of its own class, and hands its base the
  same pointer as the base every scope extends. Walking the edge is reading that member, and nothing
  is cast down anywhere. It was first built as a call to the runtime asking a scope for its parent,
  answered as the base every scope extends and cast at each use. The measurement above ran with
  link-time optimization, which inlines that call; a build without it paid a call per scope climbed,
  on every activation of a process reading out of a block. The block stores the member before its
  base is constructed; nothing in base construction reads it, and an object is created whole before
  it is constructed
  ([an-object-is-created-whole-then-constructed](an-object-is-created-whole-then-constructed.md)),
  so the order only ensures nothing ever sees it unset. The block never outlives the scope that
  built it, which owns it. A class an instance declares keeps that instance as the base instead
  ([class-declared-in-a-structural-scope](class-declared-in-a-structural-scope.md)), because what
  its unit publishes of it may name no class the instance keeps to itself.
- So the edge is held twice: the runtime's tree keeps every scope's parent for what only the runtime
  answers by walking the tree -- a scope's name (`%m`, a dump, the DPI scope registry) and where an
  upward name searched by class starts (LRM 23.8) -- and a block keeps the same scope as its own
  class for the names it reaches. Both are written once, while the block is constructed, from the
  one value its builder hands, and nothing writes either again, so they cannot come apart. Which
  scope owns a built scope is not a third statement of it either: attaching a scope to the tree
  files it under the parent it was placed under, as SystemC files an object under the parent it was
  made in, so owner and parent are one fact. Holding the edge once was weighed and rejected. In the
  runtime's base alone, the design's code would have to read a prebuilt library's private layout and
  cast down, or call it, which is what this removed. In the design's classes alone, the runtime
  would ask each scope for its parent through a virtual every scope class overrides, which moves the
  upward half of the tree out of the runtime that owns, names and walks the tree, to save a pointer
  per block. Clang keeps a declaration's semantic and lexical contexts in one slot where they agree
  because both are one generic pointer type; here the two sides of the library boundary hold
  different types.
