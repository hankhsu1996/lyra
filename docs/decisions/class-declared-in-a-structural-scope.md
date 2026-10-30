# A class declared in a structural scope belongs to one instance of it

Date: 2026-09-09 Status: accepted

2026-10-30: Decision 5 added. Decisions 1 to 4 placed a class's storage per instance and left its
code in the class, compiled into the unit's one body, which held only while a scope declaring a
class was a class of its own per specialization and per place. Decision 5 puts the code in the body
of the scope declaring the class, so that rule is gone.

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

**Each class of a lineage records its own.** A class may extend one declared in a scope enclosing
its own, and a name in the base's methods is searched outward from where the base was written (LRM
23.9), whatever class extends it. So an object whose class sits in a generate block and whose base
sits in the module holds two instances: the block's, for the derived class's bodies, and the
module's, for the base's. The second is the first climbed as many scopes out as the base's declaring
scope sits from the derived's, a count fixed where the two declarations are written. Java settles
its inner classes the same way (JLS 8.8.7.1): a superclass's enclosing instance is the n-th
lexically enclosing instance of the one being constructed, with n taken from the declarations. So do
nested procedures in the Algol family, through static links: the caller hands the callee its link by
following a count of links fixed at compile time by the difference in nesting depth. In both it is
the side doing the construction or the call that climbs, since only it holds the starting point, and
the declaration is what fixes the count. This was first written as though every class of a lineage
shared one declaring instance, and the base was handed the derived's -- which failed to compile on
one backend and read the wrong scope on the other.

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
- the constructor: the parameter the instance arrived in, from its first expression to its last.
  Entering the base comes before the object records anything (LRM 8.7), so the member is not yet
  written when the instance handed to the base and any `super.new` argument naming the declaring
  scope are evaluated;
- a body in a namespace unit: none, because its names resolve against the unit rather than an
  object.

**The parameter is a binding of the body, not a bare local.** It is seeded under its own identity,
as the receiver is, so a process the body forks (LRM 9.3.2) reaches the instance by capturing it the
way it captures anything else it names. Seeded as an anonymous local instead, it was invisible to
capture, and a static method forking a read of its declaring scope failed on both backends.

**What a body takes ahead of its formals is stated once, by the class, and read by both ends.** A
class states whether it belongs to an instance, and from that and the kind of callable follows what
each of its callables is handed: the object for an instance method, the object and then the instance
for the constructor, the instance alone for a type-associated method, nothing for any of them in a
class a namespace unit declares. The body binds exactly that, and every construction and call of the
class passes exactly that, taking from the call site only how far out the instance sits. C++ does
the same with `this` -- one arrangement of a member function, derived from its declaration, serves
its definition and every call of it -- and a disagreement between the two ends is a compiler bug
that is refused where it is found. This was first answered twice, the callee reading it off how its
body happened to be walked and each call site off its own measurement, and the two agreeing only by
coincidence is how a base came to be handed the wrong instance.

**The reference vocabulary does not change.** `../architecture/reference_resolution.md` splits a
reference into a receiver resolved on the lexical axis and object-graph hops resolved on the other,
and states that the two never merge into one distance. The hop from an object to its declaring
instance is resolved once per body, not once per reference, so it is part of seeding the body's
bindings and never a step of a route. A route still begins at a receiver and climbs typed parent
edges from there; what changed is only which binding that receiver is.

This is what keeps the decision out of every consumer. Which of the cases above applies is settled
once, where the body is built, by the code that already knows what kind of body it is. No route
consumer asks whether it is inside a class.

## Decision 3: type-associated storage is placed by what replicates the declaration

A static property's cell belongs to whatever replicates the class declaration:

| The class is declared in                | Cells                          |
| --------------------------------------- | ------------------------------ |
| a package or the compilation-unit scope | one, program-global            |
| a structural scope                      | one per instance of that scope |

This is the same sentence as `variable-lifetime-storage.md`'s rule for a static-lifetime local, one
level up, and it uses the same placement vocabulary rather than a second one. Where a structural
scope replicates the class, the cell is a field of the class the scope published, placed from the
class's declaration alone, so every body realizing the scope places it alike. The declared
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
rule is applied once more.

The specialization is a type of each instance of that scope too (LRM 8.25: a specialization on a
different type is a different type), so it is replicated by the scope its argument is, holds its
static properties per instance, and is held by that scope's unit rather than by the package
declaring the generic (`a-specialization-is-held-where-its-arguments-are.md`). This was first
written with the specialization in the package, one artifact with one set of static properties for
every instance, on the ground that the compiler knows exactly one argument class; it knows one per
unit, and the type is one per instance. A scope-declared class is nameable only inside the scope
that declares it, so every call site that can exist has an instance to supply.

This is stated because the case invites the opposite conclusion. Refusing it would be rejecting a
correct program because the per-unit shared form could not carry a value -- the shape
`north_star.md` names as making an optimization a correctness precondition.

## Decision 5: a class's code belongs to the body of the scope declaring it

A class declares what operations it has; the body of the scope declaring it implements them; the
instance an object belongs to selects whose implementation runs. A unit is compiled to one body per
distinct code (`a-unit-is-compiled-to-one-body-per-distinct-code.md`), and the class's code is the
code of whichever body compiled it -- a method reading a value one specialization folds is two
methods in two bodies -- while every referrer, another unit's class extending it included, compiles
against the class as the unit declared it and never against a body.

- **HIR.** The unit's class registry holds each class's declaration, one per class whichever copy of
  its text was elaborated. The code of each class -- its methods, constructor, initializers,
  constraints, a covergroup's bodies -- is held by the structural scope replicating it and compared
  with the rest of that scope, so two bodies differing only there are two bodies.
- **MIR.** The class is stated alike by every body: its layout, and one entry per operation, which
  reads the instance (the object's member, or the instance a construction or a static method is
  handed) and dispatches on it. The class the scope published introduces a virtual method per
  operation of each class it declares, after the subroutines it published, in an order the
  declarations alone fix. Each body realizing the scope overrides each with the code it compiled: a
  method of the body's class, the instance as its object and the operation's own object, if any,
  after it.
- **Backends.** The class is defined by every body alike and kept once by the link
  (`mir::IsStatedByEveryBody`); the code is the body's own.

A call through a handle to a class of a scope whose body it does not know goes through the entry,
and so does a construction or a static call made in the declaring scope itself. Two calls instead of
one, for a construction and a static call, are what removing the per-specialization class costs,
against an artifact per specialization and per loop index. Where an operation is a task, the entry
is a coroutine awaiting the override, one frame more per call.

The survey: rustc keeps an item's signature and its bodies under different owners (`hir::OwnerNodes`
holds the bodies an item's HIR contains, and `fn_sig` is a query of its own), as HIR here keeps the
declaration in the unit and the code in the scope. clang compiles a class nested in a class template
once per instantiation, its methods instantiated into that specialization; that is the rule this
replaces, and the condition that differs is that a C++ referrer names the instantiation, while a
referrer here names only what the scope published -- which body an instance is, is construction
data. A `dyn Trait` call in Rust reaches the concrete type's method through the object's table
(`rustc_codegen_ssa/src/meth.rs`); here the table dispatched on is the instance's, not the class
object's, because a class of another unit extending this one fills its own table without knowing any
body.

## Rejected alternatives

- **A class per specialization of a scope declaring one, and per place of a block declaring one**
  (the rule Decisions 1 to 4 rested on). A loop's blocks compile a class each, and a specialization
  differing only in code is an artifact of its own.
- **A run-time binding object per (class, instance), holding the implementations.** Every field it
  would hold is held already: the instance by the object's member, the implementations by the
  instance's own class.
- **The body's override forwarding to a type-associated implementation taking the object first.** It
  buys a call that skips the instance, and the one place a call could skip it, the declaring scope's
  own code, has the instance at hand; a task through it is one coroutine frame more.
- **MIR always dispatching, and the layer below calling directly where it can tell the target.**
  What allows a direct call is which instance an object belongs to, which no type proves.
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
