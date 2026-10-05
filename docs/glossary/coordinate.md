# Coordinate

**Definition.** The resolved form of a source name that reaches into an object: the class that
declares the named property or introduces the named behavior, together with the position it occupies
among what that class declares.

**Contrast.** Not a name -- a coordinate has already been resolved, and nothing that reads one
performs a lookup. Not an address: a coordinate says what an access reaches, independently of which
object it is applied to, so the same coordinate serves every object of the class and survives the
reference being re-pointed. Not a physical offset or table index, which is layout and belongs below
MIR. Not an absolute position in a flattened lineage; a coordinate names the class that introduced
the thing, never a position counted across everything an object inherits.

**Usage notes.** A coordinate is formed where the referrer compiles, by reading what the class
publishes, against the class the reached storage was declared with; every class a referrer can
reach, a class a design element declares included, is one some unit publishes. It is never formed
against the class an object turns out to be -- which object a reference names is not settled when a
coordinate is formed, and would be the wrong answer if it were (IEEE 1800-2023 8.14).

What the position means depends on what is named, and the cases are not interchangeable (see
`architecture/reference_resolution.md`). A property coordinate is complete once formed. A virtual
behavior coordinate names the dispatch position the source name means, and the object answers which
body fills it at the moment of the call -- so forming one is never choosing the override.

A coordinate naming the wrong class is not caught by anything downstream -- it addresses whatever
occupies that position in some other class's layout -- which is why which class it names is stated
rather than inferred.
