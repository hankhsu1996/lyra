# A class declared in a structural scope belongs to one instance of it

Date: 2026-09-09 Status: accepted

## Why this decision matters

A class declared inside a module, an interface, or a generate block is a scope in the name tree, and
its bodies name that scope's declarations. Lyra compiles a unit once and elaborates many instances
of it, so "which instance's storage" is a question the compiled class cannot answer on its own.
Getting it wrong is not a crash: it is one shared cell answering for every instance, which is
correct wherever the declaring scope happens to be instantiated once and silently wrong the moment
it is not.

Three things stood on the missing rule -- two aborts and one wrong answer nobody had reported -- and
the wrong answer is why this is a decision record rather than a bug fix.

## What the standard requires

**LRM 23.9** lists Classes among the elements that define a scope, alongside tasks and functions,
and states the upward search: a name referenced without a hierarchical path is searched outward, and
"if the item is a variable, it shall stop at a module boundary; if the item is a task, function,
named block, or generate block, it continues to search higher level modules until found. This fact
means that tasks and functions can use and modify the variables within the containing module by
name."

**LRM 6.22** decides which copy: "The scope of a data type identifier shall include the hierarchical
instance scope. In other words, each instance with a user-defined type declared inside the instance
creates a unique type."

Together: a class declared in a structural scope is replicated by that scope, and an object of it
belongs to the one instance it was created in.

**LRM 8.9** then settles type-associated storage the same way. A static property is one cell owned
by the type; the type is replicated per declaring instance; so the cell is too.

## Decision 1: the object records the instance its class was declared in

A class whose declaring scope is a structural scope carries the instance of that scope as an
ordinary borrowed member, set during construction and never reassigned.

**Borrowed, not owning or managed.** The declaring instance is built during elaboration and lives
for the whole simulation, so the reference carries no lifetime obligation. An owning or managed edge
would make every instance a root of everything its objects reach, which is a different lifetime
contract, not a conservative version of this one.

**One compiled class, not one per instance.** LRM 6.22's "unique type" is a type-identity rule the
front end enforces -- assigning between two instances' versions is rejected where the design
elaborates -- and Lyra never sees two of them, because it compiles the unit rather than the instance
graph. Emitting a class per instance would make compile-time work scale with instance count, which
`../architecture/north_star.md` invariant 2 forbids by name.

## Decision 2: a body materializes its enclosing-scope binding at entry; routes are unchanged

Every callable body already holds a materialization of each binding it names, seeded at entry. The
instance a body's outward references start from is one such binding:

- a process or subroutine of a structural scope: its receiver, as today;
- an instance method of a class the scope declares: read from the member Decision 1 adds;
- a receiver-less callable that needs one: an ordinary parameter its callers supply;
- a body in a namespace unit: none, because its names resolve against the unit rather than an
  object.

**The reference vocabulary does not change.** `../architecture/reference_resolution.md` splits a
reference into a receiver resolved on the lexical axis and object-graph hops resolved on the other,
and states that the two never merge into one distance. The hop from an object to its declaring
instance is resolved once per body, not once per reference, so it is part of seeding the body's
bindings and never a step of a route. A route still begins at a receiver and climbs typed parent
edges from there; what changed is only which binding that receiver is.

This is what keeps the decision out of every consumer. Which of the four cases above applies is
settled once, where the body is built, by the code that already knows what kind of body it is. No
route consumer asks whether it is inside a class.

## Decision 3: type-associated storage is placed by what replicates the declaration

A static property's cell belongs to whatever replicates the class declaration:

| The class is declared in                | Cells                          |
| --------------------------------------- | ------------------------------ |
| a package or the compilation-unit scope | one, program-global            |
| a structural scope                      | one per instance of that scope |

This is the same sentence as `variable-lifetime-storage.md`'s rule for a static-lifetime local, one
level up, and it uses the same placement vocabulary rather than a second one. The declared
initializer (LRM 10.5) runs once per cell, which for the second row is once per instance during that
instance's initialization -- still before any `initial` or `always` procedure, as 10.5 requires.

A static method of such a class has no receiver (`../architecture/object_model.md` invariant 7
admits none), so it takes the declaring instance as an ordinary parameter. A parameter is not a
receiver; the invariant forbids fabricating a receiver for a callable that has no object, not
passing a value a callable needs.

**The two halves of this decision cannot be separated.** Moving the cells without handing the
instance to a receiver-less method leaves that method unable to reach what it owns, which is not a
worse answer but no answer at all -- the two were tried in that order and the first alone broke
every static method of a scope-declared class.

**A cell's name has to be unique in the pool that holds it.** A callable name and a declaration id
are unique among one class's own cells; once a structural scope's pool holds the cells of every
class it declares, two classes with a like-named block collide there. The name a cell takes is
therefore qualified by what shares its pool -- a defect only the emitted text shows, since two cells
with one name are two perfectly good arena entries until something renders them side by side.

## Decision 4: the requirement propagates through specialization

A generic class specialized on a scope-declared class constructs one, so its constructing body needs
a declaring instance and obtains it by Decision 2's rule -- a member if the body belongs to an
object that outlives the call, a parameter otherwise. Nothing about this is special to generics: the
rule is applied once more, in a body that happens to live in another unit.

The specialization itself is not replicated. Its type argument is a class of the declaring unit, of
which the compiler knows exactly one, so a package generic specialized on it is one artifact. And a
scope-declared class is nameable only inside the scope that declares it, so every call site that can
exist has an instance to supply.

This is stated because the case invites the opposite conclusion. Refusing it would be rejecting a
correct program because the per-unit shared form could not carry a value -- the shape
`north_star.md` names as making an optimization a correctness precondition.

## Rejected alternatives

- **Put the class object in the runtime object tree so the existing parent edge answers.** A scope's
  parent edge is a containment relation: it is what a hierarchical name, `%m`, and the elaboration
  walk traverse. A class object is on no hierarchical path and is reached by a managed reference, so
  giving it that edge both makes it nameable where the standard says it is not and gives a managed
  object an owning parent. `object_model.md` invariant 4 keeps topology, lifetime and category as
  three axes for this reason.
- **Reach the declaring scope by lexical capture.** `../architecture/binding_and_capture.md` forbids
  reconstructing the object graph lexically. Capture is the edge across a callable boundary; a class
  declaration is not one.
- **Make the hop from an object to its declaring instance a step of the reference route.** It merges
  the lexical and object-graph axes into one distance, which `reference_resolution.md` forbids, and
  it puts a once-per-body fact into a per-reference vocabulary.
- **Pass the declaring instance to each method instead of storing it on the object.** A handle is
  stored and passed; a method may be called from a body that has never heard of the declaring scope,
  so the caller cannot be the source. This is the argument that forces Decision 1 to be a member.
- **A per-declaring-instance type descriptor the objects point at, so the instance is stored once
  rather than per object.** It trades one word per object for one indirection on every outward read.
  The read is on the simulation path and the word is not.
- **Refuse a construction reached from another unit's generic.** See Decision 4.

## Consequences

- `hir::ClassDecl`'s statement that a class "carries no structural position of its own" is false for
  this case and is replaced: a class states the scope that declares it, and a structural scope
  states the classes it declares.
- HIR-to-MIR settles every structural scope's shape before a class body lowers, because a class body
  now resolves names against the declaring scope's published shape. Class shapes are still settled
  before any body of either kind.
- `object_model.md` invariant 8 is scoped rather than reversed. "One cell owned by the type" holds;
  what it did not say is how many times the type is replicated, and its wording read as
  program-global. Decision 3 states the missing half.
- A class body reaching its declaring scope stops aborting -- in two places, since calling that
  scope's subroutine broke a different invariant than reading its variable did.
- A class's declarations settle before any body lowers, on both sides of HIR: the scope that
  declares a class states so, and drives both halves. The pool a body's procedural scopes are minted
  into may not move between the two halves, its address being what a lookup matches on.

## Cross-references

- `../architecture/north_star.md` -- invariant 2 (compile per unit, per-instance data at runtime
  construction) forces Decision 1's shape; invariant 3's forbidden shape forces Decision 4.
- `../architecture/object_model.md` -- invariants 4, 7 and 8.
- `../architecture/reference_resolution.md` -- the two axes and the rule that they never merge.
- `../architecture/binding_and_capture.md` -- per-body materialization, which Decision 2 is an
  instance of.
- `variable-lifetime-storage.md` -- the same placement rule one level down.
- `object-model-storage.md` -- one declaration storage for every object type.
