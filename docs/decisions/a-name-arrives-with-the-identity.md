# A Name Arrives With The Identity

## Date

2026-09-16

## Status

Accepted. Extends [declarations-before-bodies](declarations-before-bodies.md) D4, which settles that
an identity may precede its content and does not say what remains askable in between.

## Why this decision matters

A compilation unit mints an identity for a declaration before that declaration is settled, because a
class variable may be declared before the class itself is and two classes may each hold a handle to
the other (LRM 8.27). The pool serving that gap answers exactly one question during it -- whether
the value has landed -- so every other question waits for the whole value, including the one
question that has an answer the entire time: what the declaration is called.

Two legal programs aborted on this. A unit's published surface names a class of its own by the pair
that identifies it anywhere, the declaring unit and the class's name, so publishing a handle to a
class the unit declares asked for the class's whole declaration in order to read one field of it --
at the one moment the declaration did not exist yet. `class Node; Node next; endclass` in a package
is enough, and so is LRM 8.27's own worked example.

The neighbouring failure has the same shape without the same mechanism. A namespace publishes the
names its subroutines answer to, and it did so after walking some of the bodies that spell them, so
a package whose variable initializer called a function that package declares aborted while a package
function calling the same function ran.

Writing the rule down once is what keeps the next asker from re-deriving it, because both defects
read as ordering accidents at their own sites and neither one is visible from the other.

## Decisions

### D1. A declaration's name is part of its identity, not part of its contents

Where a pool mints identities ahead of values, the name the source gives a declaration is recorded
with the identity. Asking what a declaration is called is therefore answerable from the moment the
source names it, and never depends on whether the declaration has been settled.

The name is held in one place. A declaration does not carry its own name beside the pool's, because
two homes for one fact is what lets a rename reach one of them.

### D2. Every name a scope declares is published before any body that scope owns is walked

A scope's bodies are its subroutines, the initializers of the variables it declares, and the bodies
of the classes it declares. Any of them may name any declaration of that scope, in any direction
(LRM 13.7, 8.27), so the set of names is complete before the first of them is walked -- not before
the first of one kind of them.

### D3. What a signature says about a class is its name, and the signature is published before any body lowers

A unit's signature is what other units compile against, so it exists before any unit's bodies are
lowered. A class of the unit appears in it as the declaring unit and the class's name, which is what
identifies that class from anywhere (LRM 26.3), and never as anything about the class's contents.

D1 is what makes D3 satisfiable. A class's declaration is settled with the bodies it owns, so the
signature and the declaration cannot both come first; the name is the one part of the class the
signature needs, and it is available before either.

## Rejected alternatives

- **Settle every class's declaration before publishing the unit's signature.** The ordering fix, and
  no order exists: a body of one unit may compile against another unit's signature, so every
  signature precedes every body, while a class's declaration is settled alongside its own bodies.
  Making the signature wait would make it wait on the thing that waits on it. Rejected.

- **Recompute the name from the front end wherever it is needed.** The name is derived from the
  source symbol, so an asker holding that symbol can spell it again. That is one fact computed in
  two places, and it had already happened: the promise a class makes about its base recomputed the
  name that the class's own declaration was recording a few lines away. Rejected.

- **Keep the name on the declaration and let each asker check whether it has landed.** Turns one
  question into two at every site, and the second has no good answer -- an asker that finds the
  declaration missing can only wait, and there is nothing for it to wait on. Rejected.

- **Give the pool a general way to read part of an unsettled value.** Serves this case by making
  every field of every declaration reachable mid-construction, which is the observing-a-half-built
  value the staged lowering exists to prevent. The name is not a field that happens to be ready
  early; it is the one fact that is fixed when the identity is. Rejected.

## Consequences

- A pool that mints ahead of its values answers two questions rather than one: what a declaration is
  called, from the moment it exists, and what it declares, once it is settled.
- A debug dump can name a declaration that is minted and not yet settled, where before it could only
  report that something was there.
- A scope's publication of its own names is one step with every body after it, so adding a kind of
  body to a scope cannot quietly place it before the names it may spell.

## Cross-references

- [declarations-before-bodies](declarations-before-bodies.md) -- the staging this extends:
  identities first, declarations next, bodies last, and the four properties a declaration has by the
  time anything reads it.
- [unit-signature](unit-signature.md) -- what each unit publishes, and why the signature is an
  artifact separate from code.
- [cross-unit-class-translation](cross-unit-class-translation.md) -- how a class reference becomes
  the pair that names it from another unit.
