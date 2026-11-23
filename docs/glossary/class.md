# Class

**Definition.** The MIR compile-time entity that owns members, nested classes, parameters,
processes, and methods, and whose constructor builds its objects; a compilation unit's body, each
generate scope and each SystemVerilog class lower to a class governed by the same member rules.

**Contrast.** Not a _block_ (a process / method / constructor body, which owns locals and the
procedural statements that run in the simulation context). Not the _object graph_ (the runtime tree
of constructed objects); the class of a module or a generate scope is the compile-time type whose
constructor builds part of that tree. HIR calls the one a module or a generate scope lowers to a
_structural scope_ (`hir::StructuralScope`); MIR translates it to a class.

**Usage notes.** A module, a generate block and a SystemVerilog class are all classes
(`architecture/object_model.md`), and the member rules inside them are identical. A class holds
[members](member.md), nested classes, parameters, processes, and methods, but no locals and no
general procedural assignment outside its constructor block (see `architecture/runtime_model.md`). A
scope's class is reached from other units through the class it publishes, which holds its published
members; the class described here extends that one with what lowering adds (see
`architecture/emission_model.md`).
