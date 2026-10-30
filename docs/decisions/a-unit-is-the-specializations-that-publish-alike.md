# A Unit Is The Specializations That Publish Alike

## Date

2026-10-28

## Status

Accepted. Revises
[a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
D1, D4 and D5,
[a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
D1 and D2,
[a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)
D1 and D4, and [a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md)
D4 and D7.

Fact 4, D4, D8 and one rejection revised 2026-10-29: which blocks a construction built, how many a
loop counted out and which class each block is are no part of any class or unit. D4 as first written
made a block's class follow which alternative a construct inside it built, though the choice is made
from a value handed at construction; a loop whose blocks chose differently was a class per choice,
and the loop around it stated one table entry per block.

Those records named a unit before anything was declared, by predicting from the front end's record
of which parameters each constant evaluation read which values what a unit declares relies on. The
prediction needed records of our own in the front end, missed three things a signature holds (a
structure member's default, the window an explicit port expression names, a nested module's type
reading the outer parameter), and every compile checked it by comparing what the specializations of
one unit published, throwing where they differed. This record replaces the prediction with the
comparison: what referrers compile against is what a specialization publishes, so the class it is
belongs to every specialization publishing alike.

## Requirement

What another unit compiles against a part of the design is decided by what that unit can see of it,
so two applications that look the same to every reader are one class, and nothing decides in advance
which of an instance's values will matter.

## Facts it rests on

1. Every specialization declares its full signature on its own, from its own instance, and reads no
   other unit to do it (`incremental_build.md` invariant 8). Declaring one is cheap: 402 took about
   28 ms in a 1.5 s build.
2. A signature names another instance in three ways: one it holds (a member of the child's class),
   one outside it that what the design fixed for it reaches (the instance declaring a type a type
   parameter is bound to, an interface its port is bound to, a scope an upward name lands in, the
   body a nested definition is declared in), and one neither -- the interface a virtual interface
   type names.
3. An outside instance it reaches may be an ancestor, so its class cannot be known when the instance
   is compared: the ancestor's class depends on the instance's (fact 2's first kind, one level up).
4. An alternative a conditional generate did not select is not elaborated, and its text need not be
   valid for that instance's values (LRM 27.5), so nothing is lowered for it there. Which
   alternative stands is chosen from values handed at construction, as a C++ constructor's `if`
   chooses, and unlike `if constexpr`, whose discarded arm is never instantiated because its
   condition is a template argument (clang `TreeTransform::TransformIfStmt`).
5. A class declared inside a design element or one of its generate blocks has its methods compiled
   into the element's code and named by the element's class, and another unit calls them by name.
6. The front end has already folded every position where the language requires a constant; what is
   left of a parameter in the bound tree is a read a lowering evaluates.

## Decision

**D1. A specialization is its definition and everything the design fixed for it.** Every value or
type an instantiation may assign (LRM 23.10), the interface each port is bound to (LRM 25.3), where
each name written in it or below it lands once it leaves (LRM 23.8), and what a defparam, a bind or
a configuration fixes for it or below it (LRM 23.10.1, 23.11, 33.4), each named relative to the
instance. Nothing predicts which of them matter. Local parameters are left out, being written from
the rest by their own declarations.

**D2. A class is the specializations whose published structure reads alike.** Each specialization is
declared once more in a naming made to compare, in which a reference to another instance is written
by what it reaches:

- an instance it holds, by what that one publishes in turn, so classes are found bottom-up;
- an outside instance its key reaches (fact 2), by its place among those the key reaches, so two
  specializations reaching different instances of one class read alike;
- any other, by its class.

Its class is the digest of what it publishes so written, together with the class of each outside
instance its published structure actually names, which is added top-down once those are known (fact
3); what only its code reaches belongs to its bodies, and no referrer compiles against it. A class
is of one definition, so two cells of one name in two libraries (LRM 33.2.1) are two whatever they
publish. Every specialization on a cycle of classes turning on one another that the walk finds is
written by its own name, which shares nothing there and is never wrong.

**D3. A class is named by its definition and the digest of what it publishes,** or by the definition
alone where the class is nothing but the one specialization of its definition the design fixed
nothing for. The name is the class's, so every specialization of one class has it. Two classes are
one only where all that was written of them is equal, the digest only naming the class: a digest
reached by two different writings is reported, never taken for one class. The name moves where what
referrers compile against moves, and where a specialization joins the one the design fixed nothing
for; adding an instance otherwise, or editing a body, renames no class.

**D4. A generate block's class is found the same way inside its unit.** Inside its own bytes a path
is written from the block -- one inside it from where it stands, one reaching a block around it by
how far out that block is -- and a block nested in it by its class, so blocks of a loop that declare
alike are one class whatever their index. What is declared in a block -- a type, a class, a
covergroup -- is written with the block's class and no other. A generate construct inside it is
written as the source wrote it: which alternative it built, how many blocks a loop counted out and
which class each block is, is each object's construction (fact 4), so blocks building different
alternatives are one class, and specializations of one unit that built different blocks are one
unit, publishing every block class any of them built. A name reaching a block takes its class from
the block the front end resolved it to, which is built where the name is read.

**D5. A design element declaring a class is a class of its own for each specialization, and a
generate block declaring one for each place it stands.** Two specializations publishing alike may
still compile to different code, and the class's methods, named by the class, would then be defined
twice (fact 5). Anything in a key can change that code, so nothing short of the whole key keeps them
apart. This is a limit of how methods are named, not of what a class is, and it goes once a method's
implementation belongs to the body that compiled it and a call finds it through what the object, or
the declaring instance, is bound to.

**D6. A body is found by comparing HIR whole within one class, and is named for the first
specialization in the design's order that came out as it.** Only the design root's construction
tables name a body, so its name need be stable only as far as they need it; a name taken from the
least key renames the body whenever an edit hands one instance a value whose key is smaller, and one
taken from the code's digest rebuilds the root's tables on every edit of a body, which is the common
edit.

**D7. A parameter the code reads as a value is a field of the object; one it never reads holds
nothing at run time.** Which parameters the code reads is read off the bound tree (fact 6): every
name of one in an expression of the body, in a generate construct's scheme, and in a value an
instantiation assigns a child. One an object is handed is supplied when it is built; a local one
written from such a value is worked out when the object is built, and every such value must equal
what the front end settled it to. A parameter a class declares, one a nested definition reads, and
one the code of a scope declaring a class reads hold the value this elaboration settled.

**D8. Every specialization lowered, and in a compile that checks itself every specialization, is
declared again under the class's naming and must publish alike everything the class does.** It may
add the class of a block only it built, with what is declared in that block (D4). Where it publishes
anything else, grouping compared less than a signature holds, which is this compiler's defect,
reported as one.

**D9. A specialization is lowered once for what its objects are built as: its key worked out again
once the classes are known, every other instance in it named by what that one is built as, and every
block by its class.** A key names a block by where it stands, so a name an instance writes that
lands in a block of a loop around it makes each place a specialization of its own, and each would be
lowered alone into the same code. What the lowering reads through such a name is the class of the
scope it lands in, what that class publishes, and any constant it reads there, so the constant is a
part of the key too, taken from the declaration the name resolves to. Two specializations built as
one thing differ only in where their names land, at blocks of one class reading the same constants,
so they lower to the same code and build the same children. A construction names each child by what
it is built as, and the same holds of the children in turn: a name of a child's leaving the parent
is a part of the parent's key, and one landing inside the parent names the parent by what it is
built as. The design root's tables are stated by the same name, and hold a placement for every
instance. What each instance is built as is worked out after grouping, by a query of its own that
keeps its answers and its chain of keys in progress apart from the specializations', and a
specialization read through its own body rather than the one the front end shared must come out the
same, so no answer depends on which was asked first. A compile that checks itself declares and
lowers every specialization and holds it to what its class publishes and to the lowering of what it
is built as.

## Limits, each with its cause

- A design element declaring a class compiles a class per specialization, and a loop's blocks
  declaring one a class per index (D5). Cause: a method is named by its class (fact 5).
- A specialization handed a value its code only reads is lowered alone, and found to be one body
  with the others only by comparing what they lowered to (D6, D9). Cause: a value handed in is still
  folded wherever the front end folds it -- a width, a type query, a range -- and which of those
  positions the code holds is known only by lowering it.
- A body is renamed when the first specialization of it in the design's order changes (D6). Cause:
  no naming state outlives a build, and a digest of the code costs the common edit more.
- A reference into an outside instance's generate blocks, or from one block to a sibling of it, is
  written by where the block stands, so two reaching blocks of one class at different places are two
  classes. Cause: the outside instance's classes are not known when this one is compared (fact 3),
  and a sibling's not when the block is.
- A parameter read only by a local parameter whose initializer calls a function reading it is a
  constant of each specialization, so the specializations are a body per value. Cause: what a
  local's initializer depends on is read off the initializer as written, not off what it calls.
- Which specializations on a cycle are found to be on it follows the order the walk meets them.
  Cause: the cycle is found by the walk, not by a pass over the whole graph.
- A unit compiles a body per set of blocks its specializations built, each repeating the unit's own
  code. Cause: a body is lowered from one specialization with the blocks it built, and an
  alternative can be lowered only where it was built (fact 4).
- Which alternative stands is decided only by the condition the object evaluates when it is built,
  and an alternative's table entry where it was not built holds what it holds wherever it was, so an
  evaluation disagreeing with the front end's would build that alternative rather than stop. Cause:
  the condition is compiled from the expression the front end evaluated, like every constant
  expression the compiler evaluates, and is held to the standard by the conformance suite, not by a
  second statement of the answer in the data.

## Survey

- **Identity of an application.** rustc's `Instance { def, args }`
  (`rustc_middle/src/ty/instance.rs`) holds every generic argument and classifies none, and
  Verilator names a module clone by every parameter value (`V3Param.cpp`, `moduleCalcName`). D1 is
  theirs. Where the conditions differ: SystemVerilog writes a value each instance is handed and one
  fixing a type with one keyword, so a class per application would follow instances.
- **Merging equal modules.** CIRCT's FIRRTL deduplication (`Dedup.cpp`, `ModuleInfo`) hashes each
  module apart from the names of the modules it instantiates, then walks the instance graph
  bottom-up replacing each referenced name by its deduplicated one and compares hash and names. D2
  is that, with one condition of ours: FIRRTL has no upward reference, and an SV instance can reach
  an ancestor's type, which is why an outside instance is written by its place and its class added
  afterwards.
- **Folding equal code.** lld's identical code folding and LLVM's `MergeFunctions` compare whole
  after a hash narrows them, as D6 does.
- **A nested entity's enclosing context.** A rustc closure is identified by its definition and its
  parent's generic arguments, the enclosing context stated outright; CIRCT keeps hierarchical paths
  apart from the module definitions it deduplicates and rewrites them after. D9 states the enclosing
  context the same way, by the class of each scope a name passes, and keeps the place where an
  object stands to the construction.

## Rejected

- **Predicting which parameters a declaration relies on** (the records this revises). It needed the
  front end to record every constant evaluation's reads, missed what a signature holds and the
  record did not, and threw where the prediction was wrong.
- **A class per specialization, as rustc and Verilator name one.** A loop handing each child its
  index would compile a class per index.
- **Which alternatives a construct built as part of the class holding it (D4 as first written).** It
  is `if constexpr`'s shape laid over a choice made at construction (fact 4): the holding class
  splits by an answer its code computes anyway, and a loop around it states one table entry per
  block. Its objection to one class -- that a referrer would compile against an alternative the
  block it names never built -- does not hold: a name reaching an alternative is legal only where
  the front end built it, and takes the class from there.
- **A body named by the least key among its specializations.** It renames the body when an edit
  hands an instance a smaller key.
- **A local parameter supplied with the front end's value through each specialization's construction
  data.** It would make every block a loop builds carry an entry of its own, for an answer the
  object already works out from what it is handed.
- **Lowering once for a key with every handed value left out** (D9's alternative). A handed value is
  still folded into a width, a type query or a range wherever the front end folds it, so two
  specializations of that key can lower to different code, and knowing which positions the code
  holds is lowering it.
- **Sharing the lowering but rebuilding each specialization's children in its construction.** It
  needs a second walk finding the child at each place of the construction, for children that are
  built as one thing in every specialization lowered together anyway.

## Revises

- [a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
  D1 (a unit named by the values its declarations rely on), D4 (a scope declaring a class has one
  body per value its code relies on) and D5 (a value handed to a child's argument names the building
  scope).
- [a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
  D1: no parameter is an argument of a unit's name; a class is what is published. D2 holds of a
  specialization, whose instances lower alike, rather than of a unit, whose specializations may not.
- [a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)
  D1 and D4: which blocks were built is neither an input to a name nor part of what a scope
  publishes, and a value deciding what a child builds names nothing of the scope handing it.
- [a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D4 and D7: a
  block's class is what it publishes, not what its declarations spell, and a block declaring a class
  is one per place for the reason D5 gives, not because it is named by its index.
