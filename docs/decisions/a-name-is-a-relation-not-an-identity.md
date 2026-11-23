# A name is a relation the namespace holds, never an identity on the declaration

Status: Accepted, 2026-09-11.

## Requirement

Two different declarations never reach one program-wide symbol, whatever the source calls them. A
SystemVerilog identifier admits every printable ASCII character but white space (LRM 5.6.1), and an
escaped identifier is the plain one it spells, so the language leaves the compiler no character to
separate one name from the next and no word the source could not also write. Joining names with `.`
and naming what the compiler synthesized (`process_0`, `InstallPackageVariables`) produced
collisions in legal programs, among them `\p.A ` and `p` declaring `\A.B `, a method named
`constructor`, a task named `process_0`, and `a$b` beside `a_b` in a target mapping `$` to `_`.

## Choice

- **A declaration carries no name; its identity is the position it sits at.** Being reachable by a
  name is a relation its owner holds, a list pairing an identifier with what it reaches. A
  declaration the source never wrote is absent from that list. Where nothing holds the arena -- a
  member another unit promised, a class the source declared (LRM 8.3) -- the name is the identity.
- **This begins at the layer that reads the source.** The front end records the identifier a
  declaration was written under and none for one it introduces, so every layer below reads the
  absence rather than a word chosen to look unlikely.
- **A program-wide symbol is self-delimiting parts under a category, never names joined.** Each part
  carries its own extent, and the category says which kind of declaration the symbol names, so two
  kinds never meet however they were spelled.
- **What a declaration is called in a target is that target's to mint**, by a total, injective map
  from source names, and what the compiler synthesizes is minted into a range that map never
  reaches.
- **An entry another unit must reach but the source never named is reached by which entry it is.**
  The caller names a category and the unit, and the defining unit composes the same symbol from the
  same two facts, with nothing shared between them.

## Alternatives turned down

- **Reserving a separator character.** It fails injectivity. The failure rests on LRM 5.6.1
  admitting every printable non-space character in an escaped identifier.
- **Reserving a word for what the compiler mints.** It fails injectivity, on the same premise at the
  level of words.
- **Length-prefixed parts joined into one name space.** It fails separating kinds of declaration,
  since a constructor and a method spelled `constructor` still compose the same parts. The failure
  rests on the category not being part of any name.
- **Giving every synthesized declaration a name minted to look unlikely.** It fails injectivity, and
  every layer then carries a name for something nothing names. The failure rests on the alphabet
  being maximal, so "unlikely" is no property a compiler can rely on.
- **Hashing the parts.** It fails injectivity as a guarantee rather than a probability, and loses
  the parts a dump or a backtrace reads. The failure rests on a linker matching strings exactly.

## Consequences across layers

- Demangling is not wanted; the readable surface is the IR dump, which shows an identifier for what
  the source named and nothing for what it did not.
- Composing a symbol happens in one place, so the unit emitting a declaration and every unit
  reaching it arrive at the same string with no table between them.
- What the compiler records about a type rather than a declaration is named by the type's position
  the same way.
- [`identity_and_ownership.md`](../architecture/identity_and_ownership.md) states that an identity
  is never a rendering.
