# Value

**Definition.** What an expression evaluates to: a typed datum carrying no storage identity, no
address, and no lifetime of its own.

**Contrast.** Not a [storage identity](storage-identity.md), which is an entity a second name can
denote and which holds a value at each moment. Not the value's _representation_ -- how a backend and
runtime keep those bits, inline or behind a handle, owned or shared -- which the source language
does not prescribe.

**Usage notes.** The value hierarchy descends to bits: every component of a packed aggregate, every
member of a union, and every element of an array is a subvalue. The storage hierarchy does not
descend that far, so "part of a value" and "component with an identity of its own" are separate
questions, answered separately in `architecture/storage.md`. A representation owes the compiler two
facts, its extent and its alignment; everything else about it is a backend and runtime realization,
which is why revisiting how values are represented reopens nothing about what may be aliased.
