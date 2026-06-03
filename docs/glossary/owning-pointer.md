# Owning Pointer

**Definition.** A wrapper type expressing that a member owns the heap-allocated object it points to,
giving the owned child stable identity for the lifetime of the owner.

**Contrast.** Not a value held inline; an owned child object has stable identity and is not stored
by value (a moved or copied object would break references into it). Not a non-owning reference
resolved across the object graph (see `architecture/reference_resolution.md`).

**Usage notes.** An owned child member's type is an owning pointer to an
[object type](object-type.md). A vector of owning pointers gives an
[instance array](instance-array.md); the wrappers compose to any depth. The owning pointer carries
ownership only; the scope kind comes from the object type it wraps.
