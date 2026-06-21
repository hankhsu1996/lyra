# Class

**Definition.** The MIR compile-time entity that owns members, nested classes, parameters,
processes, and methods, and whose constructor builds the object graph; a compilation unit's body and
each named generate scope lower to a class governed by the same member rules.

**Contrast.** Not a _block_ (a process / method / constructor body, which owns locals and the
procedural statements that run in the simulation context). Not the _object graph_ (the runtime tree
of constructed objects); a class is the compile-time type whose constructor builds part of that
tree. HIR calls this same entity a _structural scope_ (`hir::StructuralScope`); MIR translates it to
a class.

**Usage notes.** A module and a generate block are both classes; the declaration rules inside them
are identical. A class holds [members](member.md), nested classes, parameters, processes, and
methods, but no locals and no general procedural assignment outside its constructor block (see
`architecture/runtime_model.md`).
