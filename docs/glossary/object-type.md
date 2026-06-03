# Object Type

**Definition.** A type that denotes an owned child object, in one of two forms: an _intra-unit
object_, naming a structural scope of this compilation unit (a named generate scope), or an
_external-unit object_, naming another compilation unit (a module instance).

**Contrast.** Not a _value type_ (integral, real, string, event, ...), which denotes data a process
reads and writes. Not an [owning pointer](owning-pointer.md) or vector, which are the wrappers that
compose over an object type to give storage and cardinality.

**Usage notes.** The intra-unit versus external-unit distinction is the owned child's runtime scope
kind -- generate scope versus module instance -- and is the only thing that encodes scope kind (see
`architecture/mir.md`). An intra-unit object references a scope of this unit by its local id; an
external-unit object references another unit by name, and that unit's layout is not visible across
the boundary (see `architecture/compilation_unit_model.md`).
