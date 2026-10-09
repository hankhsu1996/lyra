# A Design Element Publishes Its Declarations

## Date

2026-10-04

## Status

Accepted. Its D5 is reversed, and its D2 and D4 narrowed, by
[a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md). Reverses
[unit-signature](unit-signature.md) D1 and D5,
[hierarchical-callable-dispatch](hierarchical-callable-dispatch.md) D3, D4 and D5's second leaf,
[a-referrer-calls-rather-than-navigates](a-referrer-calls-rather-than-navigates.md) D1 to D4,
[hierarchical-reference-routing](hierarchical-reference-routing.md) D2, and the premise of
[structural-access-on-an-opaque-object](structural-access-on-an-opaque-object.md) and
[a-settled-access-is-ordinary-operations](a-settled-access-is-ordinary-operations.md), and one
rejected alternative of
[a-conditional-generate-chooses-at-construction](a-conditional-generate-chooses-at-construction.md).
Generalizes [publishing-an-owned-instance](publishing-an-owned-instance.md) from interfaces to every
design element. Reverses what the following entries left past a signature or reached by name:
[calling-a-subroutine-on-another-units-object](calling-a-subroutine-on-another-units-object.md) D5,
[front-end-semantic-boundary](front-end-semantic-boundary.md) D3's by-name segment,
[interface-port-binding](interface-port-binding.md) D3's rule that a module publishes its ports,
[procedural-storage-scope](procedural-storage-scope.md)'s registration and lookups,
[a-reference-states-its-use](a-reference-states-its-use.md)'s ends a scope answers, and
[an-object-model-fixed-at-compile-time](an-object-model-fixed-at-compile-time.md) D8. Restores the
placement rule of [published-member-placement](published-member-placement.md) D1 and D5.

## Requirement

A name that reaches a declaration of another design element is checked and compiled where the
referring code compiles, against what that element declares, so the access costs what an access to
the referrer's own member costs, a misspelt or mistyped name never reaches a run, and an edit
confined to an element's bodies recompiles no other element.

## Facts it rests on

1. Every named object -- a variable, net, event, a static of a named block or subroutine, a task, a
   function, a named block, a generate block, an instance -- is reachable by a hierarchical name
   from anywhere, to read, write, trigger, call, or disable (LRM 23.6, 9.6.2).
2. An upward name is resolved per instance, by searching the instantiating scopes, so in two
   instances of one definition it may land on different declarations of different types (LRM 23.8).
3. Every unit declares before any body lowers, and a declaration reads only its own unit.
4. A unit's object gains members while its bodies lower -- a slot per route resolved once, process
   state, the homes of closures -- and no name reaches any of them.
5. Whether a loop's blocks compile to one class or one each is decided by comparing them fully
   lowered, after every declaration was published (LRM 27.4 lets their bodies differ).
6. A DPI-C export is named by a C identifier the foreign side spells (LRM 35.4), which is not
   hierarchical name resolution.
7. The instance a class belongs to (LRM 6.22) can stand inside several of the places a referrer's
   upward names land, and which of them hold it depends on where the referrer stands; the name the
   class's type came through spells the same path below its landing in every instance.

## Survey

clang and rustc hand a referrer the whole declaration, lay the record out from it, and compile an
access to a constant offset and a call to a symbol; a name the referrer may not reach is a compile
error, and nothing resolves by name at run time (clang's `ASTRecordLayout` computed by
`RecordLayoutBuilder` from the complete declaration a header supplies; rustc's `layout_of` query
over the item a crate's metadata exports). Swift's library evolution keeps a type resilient to added
stored properties by never letting a client assume its size, which is fact 4's condition. A template
instance has its own identity and symbols whether or not its machine code is folded with another's,
which is fact 5's. Java's inner class reaches a member of its "immediately enclosing instance" (JLS
8.1.3); the name of the anchor an upward name starts at is taken from there.

Where this compiler differs: a unit adds members while lowering (fact 4) and decides late whether
blocks share a class (fact 5). Both are structural and stay true, and both are absorbed below the
names a referrer uses.

## Decision

**D1. Every scope a name can step into publishes its declarations.** A design element's instance and
each generate block it elaborates publish a class each: every data object, child instance and
interface port it declares; each static of its named blocks and subroutines and each disable target
of a named block, stated with the named blocks and subroutines it sits in; each static property of a
class it declares, stated with that class, since a class a scope declares is a type of the scope's
instance and what it keeps for itself is a cell of that instance (LRM 6.22, 8.9); each class it
declares, whole, as a package's is; for an interface, each view it declares and every name each view
defines (LRM 25.5); every subroutine's signature; and one entry per generate construct, listing the
class each block was published as, keyed the way a name selects a block -- the index value for a
loop, the label for a block that stands alone or that a conditional chose. Such a class's own
signature names the scope it belongs to. The publication is derived from the unit's own declarations
and never waits for a body.

**D2. A scope is two classes: the published part, and the realization extending it.** The published
part holds the published members first, in the order the publication states, and one method per
subroutine that forwards to the body. The realization adds what lowering adds (fact 4). A referrer
compiles against the published part alone and never needs the object's size, since the element's own
entry makes every object of it; an edit to a body changes the realization and no header. No method
of either is virtual.

**D3. A referrer reaches another element by typed steps only.** A route is an anchor, steps and a
leaf, and no part of it carries a string. A step is one element of the path the source wrote (LRM
23.6): the declaration it names, and the selects written beside that name, one per dimension of what
the name holds. A step onto an instance is the published member for it, selected; a step into a
loop's block is the construct's entry, selected by the block's position and viewed as that block's
published class; a step into a block that stands alone or that a conditional chose names the block
by its label, which needs no select (LRM 27.5), and views the construct's entry as that block's
class. A leaf is a published member, a published disable target, or a direct call to a published
subroutine. A construction of a class another instance declares, and a call of a method of that
class itself, are handed that instance by the same kind of route. That route starts where the name
the class's type came through landed, or, for a type handed down by a parameter, at the enclosing
instance it belongs to -- never at whichever landing happens to hold the instance, which differs
between instances of one unit (fact 7). A name the scope did not publish is refused where it
compiles.

**D4. An upward name starts at the enclosing instance of the class the front end's search landed on,
and that class tells the unit apart.** The anchor is one runtime query -- the nearest enclosing
instance of that class, or past the topmost a top-level instance of it -- and a static downcast. Two
instances whose upward names land on different classes are different units (fact 2).

**D5. Each block of a loop keeps its own identity; sharing lies beneath it.** Reversed by
[a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md): a block
published under its own name, with the names of blocks found to compile alike kept as aliases of one
class, made the set of names depend on a comparison of bodies, so a body edit changed a header --
the requirement above, failed. A block instance is an application of its block, named from the front
end alone, and blocks of one application that lower apart are that class realized more than once.

**D6. Nothing resolves a hierarchical name while the design runs.** No scope registers a signal, a
static or a disable target by name, no scope or class carries a table of subroutine or member names,
and the runtime has no by-name child search. A scope keeps its hierarchy segment for `%m` (LRM
21.2.1.5), and a scope class keeps its DPI-C export table (fact 6).

## Rejected

- **Resolving a name while the design elaborates, with its result cast unchecked.** A misspelt name
  or a type that differs between instances (fact 2) surfaces at run time or not at all, and every
  instance pays a registration per declaration.
- **Handing the referrer the whole class, size included.** The size moves with bodies (fact 4), so a
  body edit would recompile every referrer.
- **A declared base class realized by the element, its subroutines dispatched.** That is how one
  first version absorbed fact 5; it dispatches calls the language does not, which neither clang nor
  rustc does.
- **A promise of behaviors only, with no storage.** Every read becomes a call, and a name past what
  the promise lists falls back to a table of names.
- **Telling an upward name's unit apart by the climb's length or path.** It splits units that
  compile to the same code; the landing class is what changes the code.

## Revises

- [unit-signature](unit-signature.md) D1 derived what a module publishes from what an instantiator
  needs (LRM 23.2.1). What a referrer may name is LRM 23.6's set, so D1 is reversed and D5, by name
  past the signature, has nothing left to describe. D6 -- the signature derived from declarations
  alone -- stands and is what facts 4 and 5 rely on.
- [hierarchical-callable-dispatch](hierarchical-callable-dispatch.md) D3 held that publishing a
  module's subroutines would make the unit graph cyclic. Declarations are derived from each unit
  alone (fact 3), and two units' code reading each other's declarations is two files including each
  other's headers, not a cycle in either stage. D3 and D4 are reversed, and so is D5's second leaf,
  a disable target the scope answers for by name.
- [a-referrer-calls-rather-than-navigates](a-referrer-calls-rather-than-navigates.md) D1 to D4 are
  reversed: a referrer reads a member at its offset and calls a subroutine directly. Its requirement
  -- a change to what a unit keeps to itself moves no referrer -- holds through D2 here, and its D5,
  the element's own entry making the object, stands.
- [hierarchical-reference-routing](hierarchical-reference-routing.md) D2, a segment past the layout
  answered by name, is reversed by D3. Its other decisions stand.
- [structural-access-on-an-opaque-object](structural-access-on-an-opaque-object.md) and
  [a-settled-access-is-ordinary-operations](a-settled-access-is-ordinary-operations.md) rested on no
  signature carrying a class a design element declares. Such a class's shape is one per unit, and
  the element now publishes it, so the settled operations and the class name tables are gone.
- [a-conditional-generate-chooses-at-construction](a-conditional-generate-chooses-at-construction.md)
  rejected one member covering every alternative of a conditional, because a name reaching into an
  alternative then needs a base pointer and a cast. A loop whose blocks compile to different classes
  needs exactly that view whatever a conditional does (fact 5), so every construct holding one entry
  that a step views as the block it names is one rule, where a member per alternative would be a
  second shape for the conditional alone. The view is a static cast of a pointer the construction
  filled, so the name stays typed. That record's D1 to D3 stand.
