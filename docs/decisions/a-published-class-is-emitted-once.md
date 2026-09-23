# A class the runtime publishes is emitted once, where the library is

Date: 2026-09-23 Status: accepted

## Context

An emitted project is a set of translation units and a static library. Each unit holds one class per
scope the design describes, and each of those derives from the class the library publishes for a
scope. A unit names none of the storage that class holds -- it calls for every operation on it -- so
what a unit hands the linker should be its own classes and a set of calls.

Measured at `ebe46169` on a project emitted by the compiler, built with the recipe the project
ships:

| Translation unit |       Source |          Object |
| ---------------- | -----------: | --------------: |
| the design's own | 14,281 bytes | 2,163,024 bytes |
| the root scope's |  3,907 bytes | 1,688,384 bytes |

Of that second object, `.text` is **1,503 bytes**. The string table is **920,403** -- 55% of the
file -- and 88% of it is the mangled name of one type: the alternative set of a scope member's
storage, which no design and no emitted line mentions. There are 4,118 sections and 2,167 symbols,
of which 1,985 have vague linkage, meaning the linker will discard all but one copy of each after
every unit has already written its own.

Two probes separate the cause from everything it could be confused with. A unit that includes the
whole runtime surface and defines an ordinary function costs **1,104 bytes**. The same unit, plus
one class deriving from the published scope and built the way a design's scope is built, costs
**1,600,704**. So it is not the size of the surface, not the number of headers, and not what a
design contains: it is what deriving from one published class obliges a unit to produce.

The cause is that the published classes define every virtual function they have in the header. A
class in that position has no translation unit that owns its dispatch table, so the table, its type
information, and every member function reachable from an implicitly defined destructor are produced
in **each** unit that builds one. The rule is the Itanium C++ ABI's: a class's virtual table is
emitted in the unit defining its key function -- the first virtual member function that is not
inline -- and where there is none, "the tables are emitted in every object that references any of
them". LLVM's own coding standards state the same thing as an instruction to its authors: a class
with a vtable defined in a header "must always have at least one out-of-line virtual method in the
class", because without one "the compiler will copy the vtable and RTTI into every `.o` file that
`#include`s the header, bloating `.o` file sizes and increasing link times".

## The requirement

**Work that is the same for every unit of every build is done once, where the artifact those units
share is made.** The same sentence settled where the runtime's template instantiations are
performed; this is the other half of it, because performing them once did not stop every unit from
writing them out. A unit's artifact is as much what it costs as its compile is: a build of a
thousand units holds a thousand of them on disk at once, and one such build stopped for want of disk
with 51 units to go.

## The decision

**A class the runtime publishes states at least one virtual function that its own source file
defines.** The library then owns that class's dispatch table, its type information, and whatever its
destruction reaches, and a unit that derives from it produces its own class and a call.

The destructor is the one to use, because every published class of this kind already has a virtual
one and nothing else about the class has to change. Seven classes are published this way and all
seven take it: the garbage-collected base, a value of a class, a scope, a wait, a foreign execution,
and the two types a run can raise. The thrown pair reach a unit differently -- nothing derives from
them, and what a unit was carrying is their type information and the destructor a `catch` needs --
but the rule and its cause are the same, and an exception type gets its copy operations written out
beside the destructor because throwing one copies it.

| Object, unoptimized             |    Before |       After |
| ------------------------------- | --------: | ----------: |
| one class deriving from a scope | 1,600,704 |  **24,016** |
| the design's own unit           | 2,163,024 | **612,288** |
| the root scope's unit           | 1,688,384 | **117,824** |

Of that, the thrown pair is 984 bytes across the three units of one project -- real, and three
orders of magnitude under the rest. They are in because the rule is one rule, not because of what
they weigh.

The same probe optimized is 135,664 against **4,912**, so this is not the optimizer's question in
disguise: it is worth 67x unoptimized and 27.6x optimized, and a build mode chooses between those
two columns rather than between this decision and its absence.

**What holds this is a check rather than a convention**, because nothing about such a header looks
wrong: a defaulted destructor beside four deleted operations is what every other class here writes,
and no warning names it. One translation unit adding a single scope class to the shipped surface is
compiled and weighed, against a ceiling with an order of magnitude of room.

## What was rejected

**Building an emitted project optimized by default.** It is worth 11.2x on the object set, and it
costs build time on every edit, which is the trade `--release` already names and settles the other
way. It also leaves this in place: the copy is made whatever the optimizer then does with it.

**Making a unit include less of the surface.** Falsified by the probe above -- including all of it
and deriving nothing costs a kilobyte. The umbrella header is not what this costs.

**Asking for the instantiations to be suppressed rather than moved.** The declaration that does that
names a template specialization, and what repeats here is the implicitly defined members of ordinary
classes, which no such declaration reaches. That is a statement about these classes and not about
the mechanism: where what repeats is a template the library can name, suppressing is the better
answer and [a-published-operation-is-compiled-once](a-published-operation-is-compiled-once.md) takes
it.

**Giving the storage a form the surface does not define.** It would also work, and it is a change to
what a published class holds rather than to where its functions live; the smaller change is the one
the field already prescribes, and it fixes the dispatch table as well, which that one does not.

## What it costs

Seven destructors move into source files, two of which are new and hold nothing else. A design's
scope is destroyed through a call rather than through code inlined into its own unit, which is
teardown and not a simulation path.

**It does not buy build time at the scale a conformance case runs at**: the same project builds in
1.87 s against 1.77 s, which is noise, and its objects total 783,048 bytes against 3,904,344. The
gain is disk, and what disk buys is a build of a large design finishing at all.

## What is left

A unit still carries 78% of its symbol bytes as vague-linkage copies, and this section used to read
them as the value operations a design calls, deferred on the grounds that moving those would cost
the optimizer its view of them. Both halves were wrong and
[a-published-operation-is-compiled-once](a-published-operation-is-compiled-once.md) settles what
they were: the copies were of the library's own entries and of the families written over the value
domains, the arithmetic a design calls is defined in the library already, and a family is stated as
compiled rather than moved, which leaves its definition where an optimized build reads it.
