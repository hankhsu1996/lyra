# Storage Identity

**Definition.** The property of being an entity a second name can denote -- a variable, a class
property, a member of an unpacked structure, or an element of an unpacked array, each of which the
source language gives storage of its own (LRM 13.5.2).

**Contrast.** Not assignability: a packed part select is a legal assignment target and has no
identity, and a write to it is a write to the entity that contains it. Not the identity a write
raises an update event on, nor the expression a process waits on; the three are separate. Not a
[place](place.md), which is the path evaluated to find such an entity.

**Usage notes.** Membership follows from whether the aggregate gives each component storage of its
own, so the hierarchy stops descending at the first packed aggregate and at any union. An entity's
identity is independent of its position, of its membership in a container, and of the value its
parent currently holds -- four properties a realization must keep apart, because the language moves
one without moving the others. `architecture/storage.md` owns the rule and its consequences.
