# Opaque Object Reference

**Definition.** A reference whose target is an object and whose static view names no class, because
the class belongs to an instance rather than to a compilation unit and the referrer therefore has
nothing to name.

**Contrast.** Not a reference of unknown type -- it is a complete static type stating that values of
it are objects, and every legality question about it was answered by the front end. Not a reference
whose view is chosen at run time; what varies per instance is which class the target has, not what
the referrer may assume. Not an untyped pointer: it carries object identity and the reachability
semantics of the reference kind it is, and nothing may reinterpret it as data. Not an object without
a class -- the object carries its class, which is what lets a virtual behavior dispatch and a
checked downcast work through such a reference.

**Usage notes.** The distinction that makes the term worth an entry is between _naming_ a class and
_knowing_ one. The compiler knows the class; the referrer has no name for it, because a type
declared inside a design element is a distinct type per instance of that element (IEEE 1800-2023
6.22) and is nameable only inside the scope declaring it. Making it nameable is a source-level
change -- declare the class in a package -- not a compiler capability.

Every operation that reads nothing the class holds works unchanged: testing against null, comparing
identity, copying, assigning, and being retained. An operation that reaches a property or a behavior
needs a [coordinate](coordinate.md), formed where the instance is known.

A reference that does name a class is the same reference abstraction with a different static view
(see `architecture/object_model.md`); the two are not separate kinds, and converting toward the
view-less one preserves identity.
