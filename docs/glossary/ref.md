# Ref

**Definition.** A retained alias to a resolved storage entity, written `ref` or `const ref` on a
subroutine formal (LRM 13.5.2) or a module port (LRM 23.3.3.2), denoting that entity and never the
expression that found it.

**Contrast.** Not a reference route (`architecture/reference_resolution.md`), which is a path from a
referrer to a target across the object graph, classified per segment and sealed once. Not an
[owning pointer](owning-pointer.md) or a [borrowed handle](borrowed-handle.md), which name an object
rather than storage holding a value. Not an `output` or `inout` formal, which copies at the call
boundary instead of aliasing.

**Usage notes.** The bind resolves a [place](place.md) once, so a ref goes on denoting the same
entity while positions around it move and while the aggregate containing it is assigned whole.
Forming one is an operation on the container rather than a projection of it -- binding to an absent
associative entry allocates that entry. And it aliases without owning: the language extends an
enclosing scope to cover the processes a `fork` spawns, and refuses the program where that would not
reach, rather than letting an alias keep its referent alive.
