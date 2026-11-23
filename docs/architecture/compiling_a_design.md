# Compiling A Design

The compiler realizes this scheme in every respect but one. Units do not read the front end at once.
Declaring a unit and lowering its bodies to HIR take turns, one unit at a time in the order the
design lists its units, because the front end's lazy computation is unsynchronized; everything from
a unit's HIR onward runs beside the other units. The decision on reading the front end, invariant 6
and the forbidden shape against taking turns bind regardless.

## Purpose

A design is compiled to as few artifacts as the source and its values require, what referrers
compile against found from what each lowering declares, before any body is built and from nothing
building produces, so that an edit rebuilds what it changed and nothing else. Compiling a design and
running it are separate acts: what compiling produces can be kept, moved to another machine and run
any number of times without compiling again. This doc is the whole of that scheme in one place: what
an artifact is, what decides its name, what flows in at run time instead, how the work is spread
over a machine, and how the scheme lines up with the compilers and build systems it is modelled on.
The parts each have a contract of their own -- [`specialization_model.md`](specialization_model.md)
for what an argument is, [`incremental_build.md`](incremental_build.md) for keys, fingerprints, the
staging and the store, [`emission_model.md`](emission_model.md) for what an artifact contains -- and
this doc restates none of them.

## The model

Read a design element as a C++ class template whose parameters a design may hand values, and read
the design as a program that builds objects of it:

```cpp
// The signature every referrer compiles against: one header per distinct thing the
// element's lowerings publish, with every generate block any of them built.
class Leaf : public Scope { public: Logic<8> in, out; int K; struct odd; Scope* g; };

// One body per distinct code its lowerings come out as; compiled once, shared.
class LeafBody final : public Leaf {
  LeafBody(int K, const Table* t) : K(K) {   // runs the source's generate
    if (K % 2 == 1) g = t->block(0, 0)(this);
  }
};

// The design root, per lowering: which body it is and which body each child and block
// it builds is; K is handed by the parent's code.
u0: { make: LeafBody, blocks: { odd: LeafBody::create_block_0 } }
u1: { make: LeafBody, blocks: { odd: LeafBody::create_block_0 } }
```

Three kinds of thing, and nothing else:

| Kind              | What it is                                                                                                  | Count follows                                           |
| ----------------- | ----------------------------------------------------------------------------------------------------------- | ------------------------------------------------------- |
| Signature         | what referrers compile against: what the element declares                                                   | distinct published structures                           |
| Body              | the code: processes, subroutines, how to build its children                                                 | distinct code its lowerings come out as                 |
| Construction data | per lowering: which body it is, which body each child and generate block is, and the table it is built with | lowerings, and the blocks they build (data, never code) |

A value only statements use is handed to the object and leaves the signature alone, because a child
instance is a member of its parent's signature -- a hierarchical name reaches into it -- so a value
in the child's signature would be in the parent's, and in every signature above it. The parent holds
the child by its signature, and the body the object is answers each call, as a virtual call does in
C++ and a call through `dyn Trait` does in Rust.

A value parameter is part of the unit only through what it makes the unit publish -- a width, a
type, a constant a declaration requires. Everywhere else, the generate blocks it selects included,
it is a field the object is built with, read at run time, so a loop handing each child its index
makes one unit and, where the code comes out alike, one body, not one per index. Where an index
decides which generate blocks the child builds, the child is still one unit, publishing every block
class any index built, each block's code is compiled from a child that built it, and the loop's
blocks holding those children are one class.

## The decisions

**A lowering is a body the front end elaborated and did not share.** The front end finds an instance
alike an earlier one by its definition, its parameter values and the instances its interface ports
are bound to, and shares no body a hierarchical name leaves upward or something written elsewhere
reaches; an instance it found alike is served by the lowering of the body it points at, on its word,
and every other instance is lowered from its own body, under its own environment. Nothing of Lyra's
predicts which of an instance's values matter, because nothing of Lyra's decides which elaborations
are alike; a share the front end gets wrong is its defect, corrected there.

**What referrers compile against is what a lowering publishes, compared before any body is built.**
Every lowering declares its signature on its own, and lowerings whose signatures read alike, each
reference to another instance written by what it reaches, are one unit. A referrer reads the unit
only after every signature exists, which is the barrier the staging already has, so units still
compile in any order and in parallel after it, and nothing a body lowers to is ever the identity a
referrer reads. The unit is named by its definition, and by the digest of what it publishes where
the definition has more than one unit, so its name moves where what referrers compile against does.

**Which generate blocks an instance built is no part of what it publishes.** The alternative a
conditional selects, how many blocks a loop counts out and which class each block is are chosen from
values handed at construction, as a C++ constructor's `if` chooses, so an instance publishes each
construct as the source wrote it and a unit publishes every block class any of its instances built.
An alternative a conditional did not select is not elaborated and need not be valid for the
instance's values (LRM 27.5), so it is compiled only from the instances that selected it, and a name
reaching a block takes the block's class from the block the front end resolved it to.

**A body is found by comparing what each lowering came out as.** Lowering to HIR is cheap beside
everything after it, so every lowering is kept to HIR, and those of one unit whose HIR is equal are
one body -- a digest narrows the comparison, which is whole, as a linker folds identical sections.
The body is named as [`specialization_model.md`](specialization_model.md) invariant 7 states, for
the first lowering in the design's order that came out as it, and only that one is compiled further.
Every generate block is lowered on its own and is a body of its own, shared with every block of the
same body that comes out alike; which body stands where is the object's construction.

**The constructor runs the source's generate.** Its conditions and loops are constructor code, run
against the values the object was handed, as a C++ constructor runs its own; each block it builds is
made by the entry its construction table holds for it.

**The content of an artifact decides only whether its dependents rebuild.** A rebuilt body whose
output is unchanged stops the rebuild there.

**A unit's lowering reads the front end as though it were the only reader.** What the front end
answers a unit does not depend on which other units read it first or at once. The front end
elaborates lazily, computing a part where a reader first touches it, and a unit reads past its own
body wherever a name lands, so what the units read cannot be listed and elaborated before they
start: the front end's lazy computation synchronizes itself, as rustc's query system does -- a read
that finds its answer shares it, one that must compute it marks it in progress, and a second reader
of the same part waits. A unit's HIR exists only while that unit is in flight, so what is resident
across the wide stage is the elaborated design, every unit's declarations and the units being
lowered.

**How many units a build compiles at once is stated by whoever invoked it, and never chosen by the
build.** A compiler sees one design and cannot see what else holds the machine, and a build is often
nested inside another -- a test suite running many designs at once -- as recursive `make` is. So a
build told nothing runs one compile at a time, zero says the machine is the invoker's to use whole,
and the width reaches every stage that takes one unit at a time: lowering, compiling a unit's
module, and the host compile. What each produces is collected in the order the design lists its
units, so the program is the same however many ran. The width decides nothing about what is
produced, so an emitted project does not record it; its recipe takes a width of its own, as it takes
whether to use a precompiled header, while it records the compiler and the optimization level, which
do decide what is produced. Every compile is attempted even where one has failed, and every failure
is reported, since emitted text that does not compile is Lyra's defect and one run naming every unit
saves a run per unit.

**Running a design is building it and executing the program.** `run` builds the program as `build`
does, from the same kept results ([`incremental_build.md`](incremental_build.md)), and executes a
private copy of it in a directory of the run's own, which is removed when the run ends; `build`
copies the program to where it was asked for. One way to run a design is one thing to keep correct,
and it is the program a user ships. Linking needs a host C++ driver on either backend: the one named
with `--cxx`, and otherwise the first of `clang++` and `c++` found on the path.

## How the scheme lines up

| This scheme                                   | rustc                                                                                                 | C++                                                   | Bazel / ccache                           |
| --------------------------------------------- | ----------------------------------------------------------------------------------------------------- | ----------------------------------------------------- | ---------------------------------------- |
| Lowering = a body the front end did not share | -- (an `Instance` has no position in a hierarchy)                                                     | --                                                    | action key = digest of its inputs        |
| Unit = the lowerings publishing alike         | `Instance { def, args }`, but named by what it publishes rather than by its arguments                 | template instantiation, the same way                  | --                                       |
| A value read at run time, not in the name     | the proposed successor to polymorphization: monomorphic data, polymorphic code with a hidden argument | a constructor argument                                | --                                       |
| Signature / body                              | `type_of` and `fn_sig` / `optimized_mir`, queried apart so a body edit reaches no caller              | header / translation unit; module interface           | an action's declared outputs             |
| Generate blocks built, in what is published   | the arguments an `Instance` is monomorphized with                                                     | the branch an `if constexpr` keeps                    | --                                       |
| Values handed in at construction              | --                                                                                                    | the arguments `main` builds objects with              | --                                       |
| Equal code shared as one body                 | LLVM MergeFunctions                                                                                   | linker identical-code folding                         | --                                       |
| Output content stops a rebuild                | red/green: an equal result fingerprint is green                                                       | a byte-identical module interface (update-if-changed) | output digest is the next action's input |
| Width stated by the invoker                   | `-j`, defaulting to the machine                                                                       | `make -j`, one when told nothing                      | `--jobs`                                 |

A unit found by what its lowerings publish is CIRCT's FIRRTL deduplication, which hashes a module
apart from the names of the modules it instantiates and compares bottom-up; ours writes an outside
instance by its position and adds its unit afterwards, since a SystemVerilog instance can reach an
ancestor's type and a FIRRTL module cannot.

Where the conditions differ: a template argument is written at type level, while a parameter's value
follows the instance (a loop hands each child its index) -- so a unit is found by what the lowerings
publish, not named by their arguments, and a value only read as a value is a constructor argument. A
SystemVerilog body reaches out of its instance, so which elaborations are alike depends on where
each stands, a question rustc never has; the front end, with every scope open at once, answers it,
and its answer is read as given. The front end elaborates every instance into a folded tree of its
own, so what a declaration depends on is read off what each one declares, never predicted from the
text. Equal code is found before anything is compiled rather than at the link, because lowering to
HIR is cheap and everything after it is not, and because which body each child is, is construction
data rather than code, so a body compares without its children. And the generate blocks in what a
scope publishes are read off the front end's elaboration of each instance, where rustc names an
`Instance` by its definition and arguments alone. What an instance built is a function of its
definition and the values its generate scheme read, which the front end records, so this reads the
result of evaluating the instance's own scheme, as a constant it evaluated is read; it never crosses
into an instance the instance holds, whose blocks are that instance's own name. The cost is that a
name cannot be worked out until the front end has elaborated the instance. A build defaults to one
compile, as `make` does and cargo and ninja do not, because cargo and ninja are always the outermost
thing a person types and a Lyra build commonly runs nested.

## Other models, and what each fails

| Model                                                                | For                                          | Against                                                                                                                           |
| -------------------------------------------------------------------- | -------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| An artifact per instance                                             | trivially correct                            | artifacts follow instances; every instance compiled                                                                               |
| A name holding every parameter's whole value                         | the field's answer; nothing to classify      | a value following instances splits per instance                                                                                   |
| A unit's name from comparing what was lowered                        | correct without classifying anything         | what referrers compile against waits on the bodies                                                                                |
| A unit named by the parameters its declarations are predicted to use | the name before anything is declared         | needs a record of every constant evaluation's reads in the front end, and misses what a signature holds that the record did not   |
| A name from a hash of the lowered body                               | nothing predicted; a miss costs only sharing | the name is known only after building, and every edit of a body renames it; hashes merge unequal code                             |
| A body named by the parameters its code reads                        | the body before it is lowered                | a prediction: two values settling one constant are two bodies, and how an object builds has no owner                              |
| A body holding every block its instances built                       | one body whatever its instances built        | the body follows instances it is not named for; every instance handed new values is merged                                        |
| A key of everything the design fixed, worked out before lowering     | reads nothing of the front end's sharing     | a third authority on which elaborations are alike, kept right one field at a time; each miss ran another instance's code silently |
| Units reading the front end one at a time                            | no synchronization in the front end          | serializes independent work, which `north_star.md` forbids                                                                        |
| Every instance elaborated before any unit reads                      | units read a finished tree at once           | the front end's cost multiplies with the instances it would have shared                                                           |
| A width defaulting to the machine                                    | the win without asking                       | a nested build multiplies its width by every build around it                                                                      |
| Running a design inside the compiler, writing nothing                | no link step                                 | a second way to run one design, whose runtime is not the one a program links                                                      |

## Core Invariants

1. What a referrer compiles against -- a unit's name -- is a function of what the unit's lowerings
   declare, and of nothing produced by building them.
2. A lowering reads of an instance only its body and its environment, so what it comes out as is a
   function of the elaboration the front end made for that instance.
3. Which generate blocks an instance built, and how many a loop counted out, is no part of what it
   publishes, and a value read as a value reaches the object when it is built.
4. The count of code artifacts follows distinct bodies; construction data follows lowerings and the
   blocks they build; units follow distinct published structures.
5. Which body a lowering is, is found by comparing its HIR whole, a digest only narrowing the
   comparison, and nothing but the design's construction data reads the answer.
6. What the front end answers a unit's lowering does not depend on which other units read it, or
   when, and units read it at once. _Requirement: units compile in parallel
   ([`north_star.md`](north_star.md) invariant 4), and the front end is read by every unit._
7. How many compiles a build runs at once is stated by its invoker, is one where nothing states it,
   and changes nothing the build produces.
8. What compiling produces is complete without the process that compiled it: a program runs, and is
   kept and run again, with nothing of the compiler alive.

## Forbidden Shapes

- A unit's name decided by comparing or hashing what a body was lowered to. It makes what referrers
  compile against wait on the bodies, which breaks compiling referrers first (invariant 1).
- A body whose content depends on instances it is not named for, such as one holding what any of
  them built. Adding an instance elsewhere then changes it (invariant 3).
- A parameter's value in a unit's name where the value changes nothing the unit publishes. A value
  that follows the instance then makes signatures follow instances (invariant 4).
- Two bodies taken as one on a digest alone. Bodies that differ become one wherever the digest
  misses a field or collides (invariant 5).
- A parent's code naming which body a child or a block is. Lowerings sharing that code may build
  different ones (invariant 5).
- A second answer to which elaborations are alike beside the front end's: a key worked out before
  lowering, or a lowering of an instance the front end found alike an earlier one through its own
  body to check the front end (invariant 2).
- Units taking turns to read the front end, or a lock around it. It fails invariant 6 and
  [`north_star.md`](north_star.md)'s ban on choices that force global serialization of independent
  compilation work.
- A width the build chooses for itself, a width written into a design's manifest, or a width baked
  into an emitted project. It fails invariant 7: each states a fact about one machine in something
  read on every machine.
- A program run from state the compiling process holds -- records built after compiling and looked
  up there, an execution session standing in for the program. It fails invariant 8: what compiling
  produced is then half a program, which dies with the compiler.
