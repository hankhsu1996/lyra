# A Callable Has One Identity in a Unit

## Date

2026-11-15

## Status

Accepted. Revises D3 of [a-class-has-one-identity-in-a-unit](a-class-has-one-identity-in-a-unit.md),
which named a method by its class and its position in the class.

## Why this decision matters

A program names a callable in three ways: it calls it, it takes its address, and it hands its entry
to the runtime library, which enters it on an object it does not know. Each of the three has to say
which callable, and a callable is declared by many things -- a class (its methods, processes,
lifecycle bodies and constructor), the unit's namespace (a package's subroutines, a DPI-C import, a
body synthesized for a type), a closure (the body it is invoked through), and a structure or union
(the body answering one operation on its whole value). When each of those kept its callables in a
table of its own, every node naming a callable named the table as well, and each new node that had
to name one copied the same set of alternatives.

## Survey

- clang names a function by its declaration. `DeclRefExpr` holds one `ValueDecl*`
  (clang/include/clang/AST/Expr.h), a `FunctionDecl` or a `CXXMethodDecl` alike, and a method's
  class is its `DeclContext`, a property of the declaration. A call's callee and an address-of are
  the same node. A `FunctionDecl` exists whether or not it has a definition.
- rustc names a function by its `DefId`. A path to a free function or an associated one lowers to a
  zero-sized constant of type `FnDef(DefId, args)` (rustc_mir_build/src/thir/cx/expr.rs,
  `convert_path_expr`), and the owner is `tcx.parent(def_id)`. Shims and drop glue appear only below
  THIR, as `Instance`.
- Both number an unnamed body within its parent: clang's lambda mangling counts lambdas per
  declaration context, and rustc's `DefPath` gives each child a disambiguator under its parent.
- Where Lyra differs: a unit names another unit's declarations only through what that unit
  published, by name. So another unit's callable is a declaration in this unit's table that says
  which unit defines it, as clang declares an external function in each translation unit, rather
  than a reference into the other unit's numbering. Each body of a unit is lowered as its own MIR
  unit, so within one MIR unit an identity names one implementation.

## Decisions

### D1. A unit holds one table of callable declarations, and each says who declares it

Every callable the unit declares is one entry of one table. The entry states its owner -- the
namespace, a class, a class's construction, a closure, or a structure or union and the operation the
body answers -- and, inside the owner where the owner has places, its place among that owner's
callables, as rustc's `DefKey` states a definition's parent and its disambiguator; the namespace and
a class have places, and a construction, a closure's body and a record's method are each the one of
their kind, so no owner can be spelled with a place it lacks. The rest of the declaration states
whether this unit defines it: a body here, a foreign definition, a behavior left abstract, or
another unit's definition. A construction is an owner of its own kind, as clang's constructor is a
declaration kind of its own, so nothing asks a class whether a callable is its constructor.

The unit stamps the owner when the identity is taken, and no producer writes one. The next place an
owner hands out comes from a count kept for that owner, as rustc's `Definitions::create_def` hands
out each disambiguator from a count kept per parent: an allocator, never a statement of what the
owner declares. What an owner declares -- a class's callables and its construction, the namespace's
own, a closure's one body, a record's method for each operation -- is a query the unit answers from
the owners when asked and drops at every take, as rustc computes a parent's children from the
definitions, so no list stored beside the table can disagree with it. Only a class, a closure or a
record the unit has an identity for owns one, so a callable cannot be stranded on a value that is
built and then dropped. The MIR verifier refuses owners that do not cohere: a callable standing
where another stands or leaving a place empty, an owner the unit has no identity for, a second
construction of a class, a closure with no body, and a record answering one operation twice.

### D2. Everything that names a callable names its entry

A direct call, an address, a runtime entry, a dispatch row, an interface table and a behavior's slot
each name the callable by its entry and nothing else. Who declares it, its signature and its name
are read off the entry. A behavior is named by the method that introduced it.

What is not a callable of the unit keeps its own identity: an entry of the runtime library, an
operation of a type whose body is built below MIR, and a body another unit's namespace is brought up
through, which answers to no name and so cannot be declared in this unit's table without a relation
of its own. Direct, indirect and dispatching calls stay distinct operations: the entry identifies
the declaration, not how a call reaches it.

### D3. A body no name reaches is spelled by its place among its owner's callables

A symbol is spelled from names wherever the source gave one, so an identity never reaches a symbol.
A body the compiler synthesized has no name, and is spelled by its position among its owner's
callables, as clang's and rustc's disambiguators number a lambda under its parent; it is local to
that owner and means nothing beside another owner's. The position is the owner's, so spelling a body
never searches its owner's callables. A construction, a closure's body and a record's method are
each the one of their kind their owner declares, and stand at no position.

## Rejected alternatives

- **Name another unit's callable by that unit's identity for it.** A unit would then depend on
  another's numbering, and an edit to that unit that moves nothing it publishes would still
  recompile every unit naming one of its callables.
- **Keep a table per owner and widen each node that names a callable.** Every new way of naming a
  callable copies the alternatives again, and each consumer chooses a table by the alternative.

## Consequences

- What a class, a record and a closure declare is read off the owners that name them; none holds
  code, and none lists what it declares -- what a class holds of one of its callables is a relation
  over it, such as the name it answers to or the callback it answers.
- A callable a peer names before its body is lowered takes its identity first and its body later,
  whichever owner declares it.
- A symbol is spelled from names and from positions among an owner's callables, never from the
  table's identities.

## Cross-references

- [a-class-has-one-identity-in-a-unit](a-class-has-one-identity-in-a-unit.md) -- a class's identity,
  which a callable's owner names.
- `../architecture/mir.md` invariant 6.
