# A Generate Block Is A Nested Definition

## Date

2026-10-09

## Status

Accepted. Reverses
[a-design-element-publishes-its-declarations](a-design-element-publishes-its-declarations.md) D5,
and narrows its D2 and D4. Revises fact 5 of that record and what
[one-body-built-at-every-index](one-body-built-at-every-index.md) decides by comparing blocks.

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
6. Which blocks lower to the same scope is known only after every body lowered.

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

**D2. An application is one published class, and a loop is an array of them.** A loop publishes its
distinct classes once each, and which of them stands at each index.

**D3. Which application a block instance is, is computed from the front end by the function that
names a unit.** The declaring unit, a referrer stepping into the block, and a child whose name lands
on it each compute it, with no table between them and before any body lowers (fact 5). Nothing a
comparison of lowered bodies finds is told to another unit.

**D4. An index enters an application through the constants the front end settled from it.** Where
the index sits in a place that decides what is declared, the expression the front end folded there
keeps a constant, and that constant is what the index decides: two indices that fold alike in every
such place are one application. Where no constant is kept, the index's own value stands in. Three
places decide nothing for an index. What a run evaluates. What selects an alternative of a
conditional, since the construction chooses it and the class holds every alternative any of its
objects selected. And the code of a process, a continuous assignment or a subroutine, since it is no
part of what the class declares. A place this reading misses is caught where the unit declares
itself: block instances taken for one application that publish different classes have their
definition declared again with every index deciding by its own value, and a remark says the sharing
was lost.

**D5. Block instances of one application that lower apart are that class realized more than once.**
Each such scope is a realizing class extending the one published class. A published subroutine is
one method of the published class that enters the body of whichever realizing class the object is,
which it asks of the object's own class record; a class realized one way asks nothing. No referrer
sees how many realizations there are, so a body edit that changes the count changes no header.

**D6. An upward name starts at the scope it landed on, which may be a generate block.** The anchor
is the nearest enclosing scope of the class of that scope, an instance or a block, and that class
tells the unit apart. A child naming the block that holds it therefore carries no index: every block
instance of the application is of one class, so the child is one unit.

**D7. A type a block declares is a type of the block's application.** A struct is named by the
applications between it and its unit. A block declaring a class is an application per index: a
class's bodies are compiled against the scope its block lowered to, and which blocks lower to one
scope is not known where the class is named (fact 6).

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

- An index that reaches a declaration through a position where the front end kept no constant makes
  an application per value, also where two values compile alike. An identity is a function of the
  occurrence, as it is for any parameter.
- A loop whose blocks declare a class is an application per index (D7).
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
- **A virtual method per published subroutine.** A referrer would then call through a table. The
  referrer knows the class; only the declaring unit can ask which of its own classes realizes the
  object, so the question is asked inside the method the referrer already calls directly.

## Revises

- [a-design-element-publishes-its-declarations](a-design-element-publishes-its-declarations.md) D5
  is reversed by D2 and D5 here. Its fact 5 stands as a fact about bodies and no longer decides a
  name. Its D2 gave "no method of either is virtual" the reason that a referrer knows the class;
  that reason holds, and D5 here adds the one question a referrer cannot ask. Its D4 named the
  anchor an enclosing instance; D6 widens it to an enclosing scope.
- [one-body-built-at-every-index](one-body-built-at-every-index.md) keeps its D2: blocks are still
  lowered and compared, and a hole in what was predicted still costs a build and never a wrong
  program. What the comparison decides is how many realizations a class has, and no longer how many
  classes there are.
