# Member

**Definition.** A `(name, type)` pair owned by a structural scope; the type is the sole carrier of
the member's storage shape and, when the member owns a child, of its child-scope kind and
cardinality.

**Contrast.** Not a _procedural variable_ (local to a process or callable body). Not a _callable_
(function, task, process, constructor). A signal, a child instance, an instance array, and a named
generate scope are all members and differ only in their type.

**Usage notes.** Because the type carries the classification, consumers read storage, scope kind,
and cardinality from the type, never from a parallel flag or naming convention (see
`architecture/mir.md`). An owned child is a member whose type is an
[owning pointer](owning-pointer.md) to an [object type](object-type.md); an
[instance array](instance-array.md) is a member whose type wraps that in a vector.
