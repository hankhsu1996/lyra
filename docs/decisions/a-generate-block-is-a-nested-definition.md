# A Generate Block Is A Nested Definition

## Date

2026-10-09

## Status

Accepted. Reverses
[a-design-element-publishes-its-declarations](a-design-element-publishes-its-declarations.md) D5,
and narrows its D2 and D4. Revises fact 5 of that record and what
[one-body-built-at-every-index](one-body-built-at-every-index.md) decides by comparing blocks. D5
and fact 6 revised 2026-10-12: which body a block is, is named before any body lowers. D4 narrowed,
D5's dispatch and the rejection of a virtual method per published subroutine reversed the same day.
D7 revised 2026-10-13: a block declaring a class has one body. D4, D5 and fact 6 revised the same
day: an index names an application or a body by its value, not by the constants settled from it. D4
narrowed and D5 revised 2026-10-14 by
[a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md):
what selects an alternative names no block, and is part of the name of the instance holding it
through what it selected; blocks of one name are one body within an instance, never across
instances, lowered once from the blocks that built each part.

## Requirement

A design element compiles to a class written in files that carry its name. What the source declares
inside it is a declaration nested in that class under the source's own name for it, compiled once
per distinct application of it, and reached from outside by the path the source writes.

Two parts of that come from the people the compiler is for and from no clause: the file a module
compiles to carries the module's name, and the emitted text is read, so it stays near the size of
the source and uses the source's names.

## Facts it rests on

1. "An instance of a generate block is similar in some ways to an instance of a module. It creates a
   new level of hierarchy", and what it declares acts as it would in a module brought into existence
   by an instantiation, except that declarations of the enclosing scope are referenced directly (LRM
   27.3).
2. A loop instantiates its block once per index, the index is an implicit localparam of each
   instance, and a named loop declares an array of block instances (LRM 27.4). A conditional selects
   at most one of its alternatives, several of which may carry one label (LRM 27.5).
3. An upward name is searched for in the scope holding the instantiation and on up through the
   enclosing scopes, generate blocks included (LRM 23.8).
4. A type declared inside an instance is a type of that instance (LRM 6.22), and the clause says the
   same of a generate block and of a module.
5. A unit is named before any body lowers, by every party that names it, from the front end alone.
6. Whether what its block declares, or its block's code, relies on an index is read off the front
   end's own record of which parameters each constant evaluation, and each type it read as a fact,
   read, before any body lowers. This fact used to say the opposite -- that which blocks lower to
   one scope is known only once every body has -- which was true while nothing recorded what an
   evaluation read.

## What was wrong

A block was published under the path to it, index included, and blocks found to lower alike were
merged afterwards with every other block's name kept as a further name of the shared class. Measured
on the tree that did this:

| Program                                                   | What it produced                                                                   |
| --------------------------------------------------------- | ---------------------------------------------------------------------------------- |
| A fifteen-line module with two nested loops, two by four  | fifteen files, ten named by a block's path in hexadecimal, eight of them one line  |
| One `assign` changed inside a loop of three               | a different forward header: three classes where there had been one and two aliases |
| A child in a loop of three naming the block that holds it | three units for the child                                                          |
| A struct declared in a loop of two                        | a struct and a class file per block                                                |
| A generate path of about a hundred and twenty characters  | a file name the file system refuses                                                |

The second row is the requirement of the record this one revises -- an edit confined to a body
recompiles no other element -- failed by that record's own D5.

## Survey

A nested class in C++ is declared in its enclosing class and defined in the same header; Java
compiles one to a file named by joining the names, and a deep nest overruns the file-name limit.
Both recalled rather than read. Verilator has no class per block: it unrolls the loop and flattens
the block into its module, naming what was inside by the path (`V3LinkDot.cpp`), and replaces a name
past a length limit with a prefix and a digest (`VName::hashedName`). rustc shortens a codegen
unit's file name the same way (`CodegenUnit::shorten_name`), over names that are the compiler's own
and never the user's.

Where this compiler differs from Verilator: the number of blocks is run by construction rather than
fixed when the unit compiles, so a block's instances are objects of a class. A digest in a file name
is what a flattened hierarchy needs, and no part of it applies once nothing is named by a path.

## Decision

**D1. A generate block is a definition nested in the scope holding it, and each block instance is an
application of it.** An application is the block as the source wrote it together with the arguments
that change what is compiled: what the index decides, and what is written elsewhere about an
instance the block holds (LRM 23.10.1, 23.11, 33.4). An index only ever read as a value is handed
over when the block is built, as a parameter read as a value is.

**D2. An application is one published class, and a loop is an array of them.** A loop whose blocks
are one application publishes that class and nothing of how many blocks there are or where they
stand: each construction of the holding scope runs the loop, so that count is a value of the
construction, and two objects of the holding class may count differently. A loop whose blocks are
several applications publishes its distinct classes once each, and which of them stands at each
index any object of the holding class counted out.

**D3. Which application a block instance is, is computed from the front end by the function that
names a unit.** The declaring unit, a referrer stepping into the block, and a child whose name lands
on it each compute it, with no table between them and before any body lowers (fact 5). Nothing a
comparison of lowered bodies finds is told to another unit.

**D4. An index enters an application by its value, where what the block declares relies on it.**
Where the front end records that something the block declares relied on the index -- a constant the
language requires, a type it read as a fact -- the index is an argument of the block's class, and
its value names the application, as a template argument does; two indices that settle every such
constant alike are still two applications. This said, until 2026-10-13, that the constants settled
from the index named it, so that two indices folding alike were one application (see
[a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
D1 for why the value replaced them). Four places decide nothing for an index. What a run evaluates.
What selects an alternative of a conditional, since the construction chooses it and the class holds
every alternative any of its objects selected; what it selected is part of the name of the instance
holding the block
([a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)).
What counts a loop's blocks out -- where its index starts, whether a block stands at it, how it
reaches the next -- since the construction runs the loop (LRM 27.4) and how many blocks an object
holds is no part of its class. And what the statements of a process, a continuous assignment or a
subroutine run, since that is no part of what the class declares; an index that code relies on names
the body the class is realized by (D5). A subroutine's arguments and result, and a variable a
process keeps for the whole run, are what the class declares: a caller passes the one and a name
reaches the other (LRM 13.4, 6.21, 23.6). This used to say "the code of a process, a continuous
assignment or a subroutine", and read that way a static variable of a named block and an argument
sized by the index each published a different class under one name. A place this reading misses is
this compiler's defect, and is found where the compile checks itself, which publishes each class
from another of its block instances.

**D5. Block instances of one application whose code relies on their index are that class realized
once per index value.** Which body a block is, is named before any body lowers, from the value of
its index where the front end records that the block's code relied on it (fact 6), as its
application is named from the index's value where what the block declares relied on it. Blocks of
one name are one body, lowered once: each of its parts from one block that built it, which is how
the body comes to hold every alternative and every loop body their constructions built. Blocks of
one name that would lower to anything else are this compiler's defect, found where the compile
checks itself. Nothing a lowered block holds is compared with another to find which body it is. Each
body is a realizing class extending the one published class. A published subroutine is a virtual
method the published class introduces and each realizing class overrides with its body, so a call
reaches the body of the class the object is however many realize it, as a virtual call does in C++
and a call through a trait object does in Rust. No referrer sees how many realizations there are, so
a body edit that changes the count changes no header.

This decision first found the realizations by lowering every block and comparing each with the
bodies found before it, which cost the square of the number of distinct bodies and was most of the
work of lowering a loop whose blocks each read a neighbour. Naming the body from the front end's
record is the same answer with the comparison removed.

**D6. An upward name starts at the scope it landed on, which may be a generate block.** The anchor
is the nearest enclosing scope of the class of that scope, an instance or a block, and that class
tells the unit apart. A child naming the block that holds it therefore carries no index: every block
instance of the application is of one class, so the child is one unit.

**D7. A type a block declares is a type of the block's application.** A struct is named by the
applications between it and its unit. A class reaches the object of the block declaring it as that
block's body compiled it -- a name its method writes is a route slot the body adds -- and is one
class of the application however many bodies realize it, so a block declaring a class has one body:
an index its code uses names its application instead. This said, until 2026-10-13, that such a block
is an application per index, because which blocks lower to one scope was not known where the class
is named; fact 6 made it known, and the block was then realized more than once with one class
reaching into each, which ended the compile as a defect.

**D8. A declaration of a unit is identified by a path of steps, each a kind and what tells it from
its siblings, and a target spells it.** Every identifier has a unique hierarchical path name (LRM
23.6), and this is that path rooted at the unit instead of the design, so it is the same for every
instance. A step is a scope the source nests the declaration in: an application of a generate block,
a class, a subroutine, a block of statements, an unpacked structure or union. The path of no steps
is the unit's own scope. A class, a structure, what a `disable` ends and the scope holding a
published variable are each named by the unit and such a path, and nothing composes the steps into
one name: an escaped identifier may hold any character (LRM 5.6.1), so a block labelled `\g::h ` and
a block `h` in a block `g` compose alike under any separator. The kind is part of a step because its
producer knows it and every target spells by it, and because one scope may give a type and a value
one name (LRM 6.22.1 c). This is rustc's `DefPath` -- a list of steps, each a namespace, a name and
a disambiguator (`rustc_hir_id/src/definitions.rs`) -- and clang walks the same chain of declaration
contexts, tagging each by kind (`UnifiedSymbolResolution/USRGeneration.cpp`). What tells a step from
a same-named sibling is data of the step and never part of its name: which one a generate block is
among the blocks of its scope under its label, none for the first (LRM 27.5 lets the alternatives of
a conditional share one), and for an application or a specialization the digest of what was fixed
for it, where rustc keeps the arguments themselves beside the path. A name bounded under a wide hash
is the trade a unit's name already takes. The C++ backend spells block steps as nested classes, so
the text reads as the source does; a class or a structure stays in the unit's namespace, and a step
keeps the source's name where that alone tells it apart and takes a spelling carrying the rest where
it does not.

**D9. A unit's scope classes are written in the file that carries the unit's name.** A declaration
has a file of its own only where another unit's declaration needs it complete ahead of its own: a
class another class may extend, a struct another holds by value. Nothing outside a unit extends the
class of one of its scopes, and a scope's class holds what it reaches in another unit as the class
every scope extends, so no header needs another unit's scope class and none is named after a path.

## Limits, each with its cause

- An index a declaration relies on makes an application per value, also where two values compile
  alike, as a template argument does.
- A loop whose blocks declare a class is an application per value of its index wherever the blocks'
  code or what they declare relies on it (D7).
- An application or a specialization with arguments is told apart by a digest of them, as a unit is,
  so the emitted name carries sixteen hex digits. How arguments are spelled is one question for
  both.
- A unit's own name is still composed with separators, and so is the path below an instance that
  names what is written elsewhere about it. Two units can therefore still compose alike where a
  source name holds the separator. Which unit one is, is a different identity from what a unit
  declares, and it is every reference's unit name and the name of its files.
- What a class specialization that is a unit of its own declares keeps the class as its first step,
  so the C++ backend spells a structure declared there from its whole path. The class reads by its
  own name inside its unit's namespace, and its step is what tells what it declares from it.

## Rejected

- **Each block published under its own path, sharing beneath.** That is the reversed D5. A body edit
  changes how many names are aliases, which is a header.
- **Deciding the application by comparing published classes.** It needs every type interned, which
  is after units are named (fact 5).
- **A file per class with the name shortened to a prefix and a digest when it does not fit.** It
  keeps the flattened name and patches its length. The name was the defect.
- **A non-virtual method per published subroutine that asks which realization the object is.** This
  stood here as the decision, and a virtual method as the rejected alternative, on the ground that
  only the declaring unit can ask which of its classes realizes the object. The object carries its
  class, and its class's table answers that in one step; asking inside a forwarding method was a
  test per realization, each an out-of-line call walking the class's bases, and the method itself
  only forwarded. Reversed 2026-10-12.

## Revises

- [a-design-element-publishes-its-declarations](a-design-element-publishes-its-declarations.md) D5
  is reversed by D2 and D5 here. Its fact 5 stands as a fact about bodies and no longer decides a
  name. Its D2 gave "no method of either is virtual" the reason that a referrer knows the class; the
  referrer knows the published class and not which class realizes the object, so D5 here makes each
  published subroutine virtual. Its D4 named the anchor an enclosing instance; D6 widens it to an
  enclosing scope.
- [one-body-built-at-every-index](one-body-built-at-every-index.md) D2: blocks of one name are one
  body, lowered once with each part from one block that built it, and are no longer compared to find
  which body they are (D5). A hole in what the front end's record shows is this compiler's defect,
  found where the compile checks itself.
