# Structural Scope

**Definition.** A scope that owns members and child scopes and whose constructor builds the object
graph; a compilation unit's body and each named generate scope are structural scopes governed by the
same member rules.

**Contrast.** Not a _process scope_ (an `initial` / `always*` body, which owns procedural variables
and assignments and runs in the simulation context). Not the _object graph_ (the runtime tree of
constructed objects); a structural scope is the compile-time class whose constructor builds part of
that tree.

**Usage notes.** A module and a generate block are both structural scopes; the declaration rules
inside them are identical. A structural scope holds [members](member.md), child scopes, parameters,
processes, and generate constructs, but no procedural variables and no general assignment (see
`architecture/runtime_model.md`).
