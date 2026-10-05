# Instance Array

**Definition.** One named member whose type is a vector of [owning pointers](owning-pointer.md) to a
child [object](object-type.md), expanding to a vector of independent child objects; a
multidimensional array nests the vector wrapper.

**Contrast.** Not a _generate construct_. An instance array is a cardinality property of one
member's type; a generate builds named child scopes as constructor-time logic. The two are
orthogonal axes (see `architecture/hierarchy_and_generate.md`): a generate scope may contain
instance arrays, and an instance array needs no generate. Neither subsumes the other.

**Usage notes.** Multiplicity is the vector wrapper alone, so the same recursion that binds a scalar
child binds an array of any dimensionality without per-form branching. Construction fills the vector
by counting the elements out. Every element takes the one assignment its instantiation wrote (LRM
23.3.2), so the child objects are alike except for their connections -- unless something written
elsewhere (a `defparam`, a `bind`, a configuration rule) reached one of them, in which case that
element is built as its own unit and the vector holds each element by the scope pointer every object
converts to.
