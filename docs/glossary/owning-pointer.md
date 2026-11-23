# Owning Pointer

**Definition.** A pointer type expressing that its holder owns the heap-allocated object it points
to, giving that object stable identity for as long as the holder keeps it.

**Contrast.** Not a value held inline; an owned object has stable identity and is not stored by
value (a moved or copied object would break references into it). Not a
[borrowed handle](borrowed-handle.md), which names the same object without owning it. Not a
non-owning reference resolved across the object graph (see `architecture/reference_resolution.md`).

**Usage notes.** A child's construction answers an owning pointer to an
[object type](object-type.md), and the runtime object tree takes it into the parent's list of
attached children, which owns it; the parent's member keeps a borrowed handle to the same object
(see `architecture/hierarchy_and_generate.md` invariant 4). The owning pointer carries ownership
only; the scope kind comes from the object type it points to.
