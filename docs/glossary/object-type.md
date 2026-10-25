# Object Type

**Definition.** A type that denotes an owned child object, naming its class by the unit's one
identity for it: the class of a structural scope this compilation unit defines (a named generate
scope), or the class another compilation unit published for its instance (a module instance), which
this unit declares and does not define.

**Contrast.** Not a _value type_ (integral, real, string, event, ...), which denotes data a process
reads and writes. Not an [owning pointer](owning-pointer.md) or vector, which are the wrappers that
compose over an object type to give storage and cardinality.

**Usage notes.** Which unit defines the class is the owned child's runtime scope kind -- generate
scope versus module instance -- and is the only thing that encodes scope kind (see
`architecture/mir.md`). The reference is one form either way; of a class another unit defines, only
what that unit published -- never what its lowering adds to the object -- is visible across the
boundary (see `architecture/compilation_unit_model.md`).
