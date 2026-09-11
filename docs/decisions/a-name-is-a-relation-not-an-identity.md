# A name is a relation the namespace holds, never an identity on the declaration

Date: 2026-09-11 Status: accepted

## Context

A SystemVerilog identifier admits every printable ASCII character except white space (LRM 5.6.1),
and an escaped identifier is the same identifier as the plain one it spells -- `\cpu3` **is**
`cpu3`. So the language leaves the compiler no character to separate one name from the next and no
word to mean something the source could not also have written.

Two habits collided with that. Program-wide symbols were composed by joining names with `.` and a
minted word: a class linked under `unit.Class`, its constructor under `unit.Class.constructor`, a
namespace callable under `unit.name`. And a declaration the source never wrote was given a name of
the compiler's own -- a process became `process_0`, a lifecycle body `ResolveState`, a package's
storage bring-up `InstallPackageVariables` -- so that the unit emitting it and the unit reaching it
could agree on a spelling.

Both are unsound over that alphabet, and the failures are legal programs rather than corner cases:

- Two packages `\p.A ` and `p` declaring `\A.B ` compose to one symbol.
- A class declaring `function void \constructor ();` collides with its own constructor's symbol.
- A module declaring `task process_0;` collides with the body of its first `always` block.
- A package declaring `function void InstallPackageVariables();` collides with the entry the design
  root calls to bring that package's storage up.
- No collision is needed for the last class of failure: `a$b` and `a_b` are different declarations
  that a target mapping `$` to `_` emits as one.

A composed symbol also cannot be taken apart, so nothing downstream can recover the parts, and
`identity-is-not-a-rendering` had already fixed the rule this violates: what must distinguish is
stored as its parts, never as a composed name.

## Decision

**A declaration carries no name. Its identity is the position it sits at; being reachable by a name
is a relation its owner holds.** A class and a unit's namespace each carry a list pairing an
identifier with what it reaches -- a body, a field, a type-associated cell, a namespace variable --
and a body of code carries one for the locals it declares. A declaration the source never wrote is
simply absent from that list rather than present with a minted spelling in it.

The two exceptions are both the same one: where nothing holds the arena, the name is the identity. A
member another unit promised carries its name, because a referrer has that unit's published
identifier and not its pools; and a class the source declared carries its name, because that is what
identifies the class across a unit boundary (LRM 8.3). Neither admits a declaration the source did
not write, so neither has a nameless entry to describe.

**This begins at the layer that reads the source, not at the one that emits.** A front end records
the identifier a declaration was written under and records none for a declaration it introduces
itself, so every layer below reads the absence rather than a word chosen to look unlikely. A rule
that started lower would leave that layer supplying names nothing downstream could tell from the
design's own.

**A program-wide symbol is composed from self-delimiting parts under a category, never by joining
names.** Each part carries its own extent -- a name as its length and bytes, an ordinal as its
digits and a terminator -- and the category says which kind of declaration the symbol names. Two
declarations of different categories therefore never meet however they were spelled, and a category
the compiler mints needs no spelling the source could also write.

**What a declaration is called in a target language is that target's to mint.** A source name is
mapped into the target's identifier space by a total, injective function, and what the compiler
synthesizes is minted into a range that function's image never reaches.

**An entry another unit must reach but the source never named is reached by which entry it is.** Not
by an agreed word: the caller names a category and the unit, and the unit that defines it composes
the same symbol from the same two facts, so the two ends agree with nothing shared between them.

## Consequences

- Demanglability is still not wanted: the readable surface is the IR dump, not the symbol. What is
  required is injectivity, because the linker matches strings and a collision is two declarations
  landing on one. `specialization-identity` decides the neighbouring question -- how a unit's
  parameter bindings are encoded into its name -- and trades exact injectivity there for a bounded
  name under a wide hash; that trade is about the encoded bindings and is untouched here. What this
  record governs is the composition around such a name, which takes no such trade.
- One position on this axis is still open: a specialization's unit name joins the definition's name
  to its binding encoding with a separator, so a source unit whose name happens to end in that
  separator and a matching encoding reaches the same string. It is the same defect as the ones above
  and the same fix applies -- the parts, under a category -- but it crosses every cross-unit
  reference, which names units by that string.
- A backend that spells a declaration in a target language answers one question -- what reaches this
  declaration from outside -- and spells the answer. It never works out which kind of declaration it
  is looking at.
- The C++ backend reserves one identifier prefix. A source name that already reads as a C++
  identifier and does not begin with it renders as itself; every other renders as that prefix, an
  escape marker, and its bytes. The two images are disjoint.
- Nothing composes a symbol outside the one place that owns composing them, so the unit that emits a
  declaration and every unit that reaches it arrive at the same string with no table between them.
- A part of a symbol says which of the two ranges it is in -- an identifier the source wrote, or a
  position counted where it wrote none -- so a declaration the source named can never compose the
  symbol of one it did not, whatever either is called.
- What the compiler records about a type rather than about a declaration is named the same way, and
  for the same reason: a unit's record of one of its packed types is reached by that type's own
  position, because a design may declare a variable spelled like anything a record could be called.
- Every mechanism that existed to keep minted names apart is gone, because a position is distinct by
  being one. What went: a rename that suffixed a colliding local until it was free, a prefix that
  qualified the cells several declarations put in one pool, and the words composed for a process, a
  continuous assignment, a port connection, an assertion and a scope's cancellation flag. The rename
  was the sharpest of them -- it checked for a collision once and never rechecked the name it
  produced, so two locals of one body could still reach one spelling.
- What the emitted text costs by this is how a synthesized declaration reads: a handle that showed
  which block it stood for is now its slot. That is the trade this record already took for an
  escaped name, and there was no source name here to lose. The IR dump is where a reader asks which
  declaration a position is, and it answers by showing an identifier for what the source named and
  nothing for what it did not.

## Alternatives considered

**Reserve a separator character.** Rejected: LRM 5.6.1 leaves none. Every printable non-space
character is admissible in an escaped identifier, so any separator is a character some program
writes.

**Reserve a word for what the compiler mints.** Rejected for the same reason at the word level: a
class may declare a method named `constructor`, a module a task named `process_0`, a package a
function named `InstallPackageVariables`.

**Length-prefix the parts but keep joining them into one name space.** Rejected. Length-prefixing
fixes ambiguity between names and leaves the second failure untouched: a class's constructor and a
method the source spelled `constructor` still compose the same parts. The category is what separates
kinds of declaration, and it is not a part of the name.

**Give every synthesized declaration a name anyway, minted to look unlikely.** Rejected. "Unlikely"
is not a property a compiler can rely on when the alphabet is maximal, and the shape leaks upward:
every layer then carries a name for something nothing names, and every consumer has to decide
whether the name it is holding came from the source.

**Hash the parts.** Rejected. A hash is not injective, so it answers the question this record is
about with a probability rather than a guarantee, and it throws away the parts that make a symbol
readable in a dump or a backtrace.
