# Place

**Definition.** An access path to storage -- a base plus a chain of projection steps -- spelled
inside the instruction that consumes it and evaluated where it is spelled.

**Contrast.** Not a [storage identity](storage-identity.md): a place locates one, and the identity a
program retains is the value the address-of operation yields. Not a [value](value.md) either -- a
place is not an operand, is not held in a local, and does not cross a control-flow edge.

**Usage notes.** Evaluating a path twice is not the same question as resolving it once, because the
base and the projections may denote something different in between; that is why nothing surviving a
bind may carry a coordinate into a container. `architecture/lir.md` owns the vocabulary -- member
and dereference steps, with no index or slice step -- and address-of is the one operation that turns
a path into something the program can retain.
