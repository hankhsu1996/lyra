# Borrowed Handle

**Definition.** A member whose type is a non-owning typed pointer to an object the runtime object
tree owns, giving the enclosing class one layout-visible step to the object it names.

**Contrast.** Not an [owning pointer](owning-pointer.md), which carries the child's lifetime; a
borrowed handle never does. Not a cross-instance reference slot, which points at a value cell and is
filled during reference resolution -- a borrowed handle points at an object and is filled at
construction, from the value the child's construction returns.

**Usage notes.** Every scalar child keeps one on its enclosing class: a submodule instance and a
generate block alike. It is typed to the child's own class in every case, which is what makes a
class's layout a complete statement of which objects the runtime builds under it. An
[instance array](instance-array.md)'s member is a vector of them, one per element, nested once per
dimension (see `../architecture/hierarchy_and_generate.md` invariant 4).
