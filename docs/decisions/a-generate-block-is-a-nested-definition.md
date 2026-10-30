# A Generate Block Is A Nested Definition

## Date

2026-10-09

## Status

Accepted. Reverses
[a-design-element-publishes-its-declarations](a-design-element-publishes-its-declarations.md) D5,
and narrows its D2 and D4. Revises fact 5 of that record and what
[one-body-built-at-every-index](one-body-built-at-every-index.md) decides by comparing blocks. Fact
6, D4, D5 and D7 revised 2026-10-12 and 2026-10-13, and D4 and D5 again 2026-10-14 by
[a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md):
what selects an alternative names no block, and is part of the name of the instance holding it
through what it selected; blocks of one name are one body within an instance, never across
instances, lowered once from the blocks that built each part. D1, D4 and D7 revised 2026-10-24: a
loop's block is named by what it declares as resolved, and one declaring a class by its index. Fact
6, D4 and D5 revised 2026-10-25, with
[a-body-states-nothing-of-another-bodys-implementation](a-body-states-nothing-of-another-bodys-implementation.md):
every block is named by what it declares, a nested construct only as written; which body a block is,
is found by comparing what it came out as, across the unit, and is its object's construction data.
The 2026-10-14 D5 named a body before lowering from which values its code read, and held a block's
nested blocks inside it; measured, the second split a loop whose blocks select different
alternatives into a body per alternative, and the first was a prediction that every value its code
reads changes the code.

D4 and D7 revised 2026-10-28 by
[a-unit-is-the-specializations-that-publish-alike](a-unit-is-the-specializations-that-publish-alike.md):
a block's class is what it publishes, found by comparing, not what its declarations spell; a block
declaring a class is one per place because its methods are named by its class, not because it is
named by its index.

D2 and D4 revised 2026-10-29 by the same record: what a block built is no part of the class holding
it or of its unit, so D4's "every alternative ... whether or not it was built" holds again, now for
units too, and a loop publishes neither its blocks' classes nor where they stand; a name reaching a
block takes its class from the block the front end resolved it to.

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
   same of a generate block and of a module. The front end holds each block of a loop to it: an
   object of a class one block declares is refused where another block's class of the same text is
   wanted ("value of type 'C' cannot be assigned to type 'C'"), and no name outside a block can name
   a type it declares ("cannot refer to type names via hierarchical reference").
5. A unit is named before any body lowers, by every party that names it, from the front end alone.
6. What a block declares needs no record of which values were read: the front end resolved each
   declaration of every block, and the resolved types are the answer. What a block's code is, is its
   HIR, which is cheap to lower beside everything after it (HIR of 64 blocks in 17 ms, everything
   after it 5.7 s, 2026-10-25).

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
that change what is compiled: what the block declares, and what is written elsewhere about an
instance the block holds (LRM 23.10.1, 23.11, 33.4). An index only ever read as a value is handed
over when the block is built, as a parameter read as a value is.

**D2. An application is one published class, and a loop is an array of them.** A loop whose blocks
are one application publishes that class and nothing of how many blocks there are or where they
stand: each construction of the holding scope runs the loop, so that count is a value of the
construction, and two objects of the holding class may count differently. A loop whose blocks are
several applications publishes nothing of which stands where: that too is each construction's, and a
name reaching one of its blocks takes the block's class from the block the front end resolved it to.

**D3. Which application a block instance is, is computed from the front end by the function that
names a unit.** The declaring unit, a referrer stepping into the block, and a child whose name lands
on it each compute it, with no table between them and before any body lowers (fact 5). Nothing a
comparison of lowered bodies finds is told to another unit.

**D4. A block is named by what it declares, as the front end resolved it.** Each declaration a name
can reach (LRM 23.6) enters under its path from the block: a variable, a net or a parameter by its
type, a typedef by what it names, a subroutine by its result and its arguments, a static variable of
a block of statements or a subroutine by its type, and an instance by its unit. A generate block or
a loop inside it enters only as written there, every alternative of a conditional whether or not it
was built: what it declares is its own name's, and which of them an object builds, and how many, is
that object's construction. A structure, a union or an enumeration the block declares is spelled by
what it holds, since its own name runs through the block's. That is all a referrer compiles against,
so two blocks that declare alike are one application whatever their index or the alternative that
built them, as rustc names a closure type it synthesizes by the types of what it captures and not by
the values (`ClosureArgs::tupled_upvars_ty`, `rustc_type_ir/src/ty_kind/closure.rs`). This replaced
naming a block by its index wherever the front end recorded that a declaration read it, "as a
template argument does": the index is no template argument -- D1 hands it over at construction --
and naming by it made one class per index where the declarations of many indices resolve alike.
Three places decide nothing for an index. What a run evaluates. What selects an alternative of a
conditional, since the construction chooses it and the class names every alternative the source
wrote; which of them it selected is no part of the instance holding the block either. And what
counts a loop's blocks out -- where its index starts, whether a block stands at it, how it reaches
the next -- since the construction runs the loop (LRM 27.4) and how many blocks an object holds is
no part of its class. What the statements of a process, a continuous assignment or a subroutine run
is no part of what the class declares; it is the body the class is realized by, found from what it
came out as (D5).

**D5. A block's body is what its code came out as, found by comparing it, and which body each block
is, is its object's construction.** Each block is lowered from its own elaboration, its nested
blocks lowered on their own beside it, and its HIR is looked up among the bodies the unit holds: a
digest of its text narrows which are compared, and it is one of them only where everything they
state is equal (fact 6). So blocks of a loop that come out alike are one body whatever their index,
and a body states nothing of its nested blocks' bodies, so blocks selecting different alternatives
of a conditional are one body too. Which body stands at each place -- each alternative a conditional
wrote, each block a loop counts out -- is how the object holding them is built: it is stated in that
object's construction table, and the object builds each block through the entry the table holds
there, as it builds an instance. Each body is a realizing class extending the one published class,
lowered against the classes the scopes around it published, which are the same for every scope
building it. A published subroutine is a virtual method the published class introduces and each
realizing class overrides with its body, so a call reaches the body of the class the object is
however many realize it, as a virtual call does in C++ and a call through a trait object does in
Rust. No referrer sees how many realizations there are, so a body edit that changes the count
changes no header.

**D6. An upward name starts at the scope it landed on, which may be a generate block.** The anchor
is the nearest enclosing scope of the class of that scope, an instance or a block, and that class
tells the unit apart. A child naming the block that holds it therefore carries no index: every block
instance of the application is of one class, so the child is one unit.

**D7. A type a block declares is a type of the block's application, and a block declaring a class is
an application of its own at every index.** A struct is named by the applications between it and its
unit. A class or a covergroup each block of a loop declares is a type of that block alone (fact 4),
and an object's class is what a cast asks of it, so the blocks' classes are compiled apart: a block
declaring one, or holding a scope that does, is named by its index, and has a body per index. This
replaced "one class of the application however many bodies realize it", which had a loop whose
declarations read no index declare one class for every block, so a cast in one block took another
block's object as its own.

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

- A loop whose blocks declare a class is an application per index, its methods included: each
  block's class is a type of its own (D7), and nothing shares the code of two classes yet.
- Every block is built through its object's table, an entry read and called indirectly, once per
  block at construction (D5).
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
  is after units are named (fact 5). D4 compares what the front end resolved instead, which is there
  before anything is named.
- **A file per class with the name shortened to a prefix and a digest when it does not fit.** It
  keeps the flattened name and patches its length. The name was the defect.
- **Naming which body a block is before it lowers, from the front end's record of which values its
  code read (this record's 2026-10-14 D5).** A prediction that every value a block's code reads
  changes the code; and its earlier rejection of comparing -- the square of the number of distinct
  bodies -- is answered by comparing only bodies whose text has one digest (D5).
- **A block's nested blocks held inside its HIR.** Two blocks then differ wherever their nested
  blocks do, so a loop whose blocks select different alternatives was a body per alternative, and
  which body a nested block is would be part of the code holding it rather than of the object.
- **Each group of alike blocks lowered once from every block in it, the group's body holding every
  nested body its members built.** A body's content would follow which objects happened to share it,
  and a member's nested body would be chosen by code no other member wrote.
- **A non-virtual method per published subroutine that asks which realization the object is**, on
  the ground that only the declaring unit can ask which of its classes realizes the object. The
  object carries its class, and its class's table answers that in one step; asking inside a
  forwarding method is a test per realization, each an out-of-line call walking the class's bases,
  and the method itself only forwards.

## Revises

- [a-design-element-publishes-its-declarations](a-design-element-publishes-its-declarations.md) D5
  is reversed by D2 and D5 here. Its fact 5 stands as a fact about bodies and decides no name. Its
  D2 gave "no method of either is virtual" the reason that a referrer knows the class; the referrer
  knows the published class and not which class realizes the object, so D5 here makes each published
  subroutine virtual. Its D4 named the anchor an enclosing instance; D6 widens it to an enclosing
  scope.
- [one-body-built-at-every-index](one-body-built-at-every-index.md) D2: which body a block is, is
  found by comparing what it came out as, and is stated per object in its construction table (D5).
