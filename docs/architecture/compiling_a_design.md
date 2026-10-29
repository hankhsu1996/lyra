# Compiling A Design

## Purpose

A design is compiled to as few artifacts as the source and its values require, what referrers
compile against found from what each specialization declares, before any body is built and from
nothing building produces, so that an edit rebuilds what it changed and nothing else. This doc is
the whole of that scheme in one place: what an artifact is, what decides its name, what flows in at
run time instead, and how the scheme lines up with the compilers and build systems it is modelled
on. The parts each have a contract of their own -- `specialization_model.md` for what an argument
is, `incremental_build.md` for keys, fingerprints and the staging, `emission_model.md` for what an
artifact contains -- and this doc restates none of them.

## The model

Read a design element as a C++ class template whose parameters a design may hand values, and read
the design as a program that builds objects of it:

```cpp
// The signature every referrer compiles against: one header per distinct thing the
// element's specializations publish, the generate blocks they built included.
class Leaf_odd : public Scope { public: Logic<8> in, out; int K; struct odd; Scope* g; };

// One body per distinct code its specializations come out as; compiled once, shared.
class LeafBody final : public Leaf_odd {
  LeafBody(int K, const Table* t) : K(K) {   // runs the source's generate
    if (K % 2 == 1) g = t->block(0, 0)(this);
  }
};

// The design root, by what each instance is built as: which body it is and which body
// each child and block it builds is; K is handed by the parent's code.
u0: { make: LeafBody, blocks: { odd: LeafBody::create_block_0 } }
u1: { make: LeafBody, blocks: { odd: LeafBody::create_block_0 } }
```

Three kinds of thing, and nothing else:

| Kind              | What it is                                                                                                         | Count follows                                                                   |
| ----------------- | ------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------- |
| Signature         | what referrers compile against: what the element declares                                                          | distinct published structures, the generate blocks built included               |
| Body              | the code: processes, subroutines, how to build its children                                                        | distinct code its specializations come out as                                   |
| Construction data | per what specializations are built as: which body each child and generate block is, and the table it is built with | what specializations are built as, and the blocks they build (data, never code) |

A value only statements use names a specialization and leaves the signature alone, because a child
instance is a member of its parent's signature -- a hierarchical name reaches into it -- so a value
in the child's signature would be in the parent's, and in every signature above it. The parent holds
the child by its signature, and the body the object is answers each call, as a virtual call does in
C++ and a call through `dyn Trait` does in Rust.

A value parameter is part of the class only through what it makes the class publish -- a width, a
type, a constant a declaration requires, the generate blocks it selects. Everywhere else it is a
field the object is built with, read at run time, so a loop handing each child its index makes one
class and, where the code comes out alike, one body, not one per index. Where an index decides which
generate blocks the child builds, the child is one class per set of blocks built, however many
indices select each set, and the loop's blocks holding those children are one class per kind of
child they hold.

## The decisions

**A specialization is its definition and everything the design fixed for it.** Every value and type
an instantiation assigns, the interface each port is bound to, where each upward name lands, and
what a defparam, a bind or a configuration fixes below it. Nothing predicts which of them matter, so
two instances of one specialization lower alike by construction; the tests that lower every instance
and compare hold the key to that.

**What referrers compile against is what a specialization publishes, compared before any body is
built.** Every specialization declares its signature on its own, and specializations whose
signatures read alike, each reference to another instance written by what it reaches, are one class.
A referrer reads the class only after every signature exists, which is the barrier the staging
already has, so units still compile in any order and in parallel after it, and nothing a body lowers
to is ever the identity a referrer reads. The class is named by its definition and the digest of
what it publishes, so its name moves where what referrers compile against does.

**Which generate blocks an instance built is part of what it publishes.** An alternative a
conditional did not select is not elaborated and need not be valid for the instance's values (LRM
27.5), so it cannot be compiled from an instance that did not select it. What was built is therefore
part of what an instance is, as the branch an `if constexpr` keeps is part of a C++ instantiation.
How many blocks a loop counts out is not part of it.

**A body is found by comparing what each specialization came out as.** Every value an instance is
handed names its specialization. Specializations that differ only in where their names land, at
blocks of one class reading the same constants, are built as one thing. Lowering to HIR is cheap
beside everything after it, so each such thing is lowered, and those of one unit whose HIR is equal
are one body -- a digest narrows the comparison, which is whole, as a linker folds identical
sections. The body is named for the first specialization in the design's order that came out as it,
and only that one is compiled further. Every generate block is lowered on its own and is a body of
its own, shared with every block of the same body that comes out alike; which body stands at each
place is the object's construction.

**The constructor runs the source's generate.** Its conditions and loops are constructor code, run
against the values the object was handed, as a C++ constructor runs its own; each block it builds is
made by the entry its construction table holds at that place.

**The content of an artifact decides only whether its dependents rebuild.** A rebuilt body whose
output is unchanged stops the rebuild there.

## How the scheme lines up

| This scheme                                           | rustc                                                                                                 | C++                                                   | Bazel / ccache                           |
| ----------------------------------------------------- | ----------------------------------------------------------------------------------------------------- | ----------------------------------------------------- | ---------------------------------------- |
| Specialization = definition + everything fixed for it | `Instance { def, args }`                                                                              | template instantiation by arguments                   | action key = digest of its inputs        |
| Class = the specializations publishing alike          | --                                                                                                    | --                                                    | --                                       |
| A value read at run time, not in the name             | the proposed successor to polymorphization: monomorphic data, polymorphic code with a hidden argument | a constructor argument                                | --                                       |
| Signature / body                                      | `type_of` and `fn_sig` / `optimized_mir`, queried apart so a body edit reaches no caller              | header / translation unit; module interface           | an action's declared outputs             |
| Generate blocks built, in what is published           | the arguments an `Instance` is monomorphized with                                                     | the branch an `if constexpr` keeps                    | --                                       |
| Values handed in at construction                      | --                                                                                                    | the arguments `main` builds objects with              | --                                       |
| Equal code shared as one body                         | LLVM MergeFunctions                                                                                   | linker identical-code folding                         | --                                       |
| Output content stops a rebuild                        | red/green: an equal result fingerprint is green                                                       | a byte-identical module interface (update-if-changed) | output digest is the next action's input |
| Every instance lowered and compared, in tests only    | `-Zincremental-verify-ich`                                                                            | --                                                    | --                                       |

A class found by what its specializations publish is CIRCT's FIRRTL deduplication, which hashes a
module apart from the names of the modules it instantiates and compares bottom-up; ours writes an
outside instance by its place and adds its class afterwards, since a SystemVerilog instance can
reach an ancestor's type and a FIRRTL module cannot.

Where the conditions differ: a template argument is written at type level, while a parameter's value
follows the instance (a loop hands each child its index) -- so a class is found by what the
specializations publish, not named by their arguments, and a value only read as a value is a
constructor argument. The front end elaborates every instance into a folded tree of its own, so what
a declaration depends on is read off what each one declares, never predicted from the text. Equal
code is found before anything is compiled rather than at the link, because lowering to HIR is cheap
and everything after it is not, and because which body each child is, is construction data rather
than code, so a body compares without its children. And the generate blocks in what a scope
publishes are read off the front end's elaboration of each instance, where rustc names an `Instance`
by its definition and arguments alone. What an instance built is a function of its definition and
the values its generate scheme read, which the front end records, so this reads the result of
evaluating the instance's own scheme, as a constant it evaluated is read; it never crosses into an
instance the instance holds, whose blocks are that instance's own name. The cost is that a name
cannot be worked out until the front end has elaborated the instance.

## Models this one replaces

| Model                                                                | For                                          | Against                                                                                                                         |
| -------------------------------------------------------------------- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| An artifact per instance                                             | trivially correct                            | artifacts follow instances; every instance compiled                                                                             |
| A name holding every parameter's whole value                         | the field's answer; nothing to classify      | a value following instances splits per instance                                                                                 |
| A unit's name from comparing what was lowered                        | correct without classifying anything         | what referrers compile against waits on the bodies                                                                              |
| A unit named by the parameters its declarations are predicted to use | the name before anything is declared         | needs a record of every constant evaluation's reads in the front end, and misses what a signature holds that the record did not |
| A name from a hash of the lowered body                               | nothing predicted; a miss costs only sharing | the name is known only after building, and every edit of a body renames it; hashes merge unequal code                           |
| A body named by the parameters its code reads                        | the body before it is lowered                | a prediction: two values settling one constant are two bodies, and how an object builds has no owner                            |
| A body holding every block its instances built                       | one body whatever its instances built        | the body follows instances it is not named for; every instance handed new values is merged                                      |

## Core Invariants

1. What a referrer compiles against -- a unit's name -- is a function of what the unit's
   specializations declare, and of nothing produced by building them.
2. A lowering reads of an instance only what its specialization's key holds, so every instance of
   one specialization lowers to the same code and builds the same children.
3. Which generate blocks an instance built is part of what it publishes; how many blocks a loop
   counted out is not, and a value read as a value reaches the object when it is built.
4. The count of code artifacts follows distinct bodies; construction data follows specializations
   and the blocks they build; classes follow distinct published structures.
5. Which body a specialization is, is found by comparing its HIR whole, a digest only narrowing the
   comparison, and nothing but the design's construction data reads the answer.

## Forbidden Shapes

- A unit's name decided by comparing or hashing what a body was lowered to. It makes what referrers
  compile against wait on the bodies, which breaks compiling referrers first (invariant 1).
- A lowering reading a fact the front end folded opportunistically -- a range it handed up, a branch
  it left dead, a read its analysis dropped. Two instances of one specialization then lower
  differently (invariant 2).
- A body whose content depends on instances it is not named for, such as one holding what any of
  them built. Adding an instance elsewhere then changes it (invariant 3).
- A parameter's value in a unit's name where the value changes nothing the unit publishes. A value
  that follows the instance then makes signatures follow instances (invariant 4).
- Two bodies taken as one on a digest alone. Bodies that differ become one wherever the digest
  misses a field or collides (invariant 5).
- A parent's code naming which body a child or a block is. Specializations sharing that code may
  build different ones (invariant 5).
- A check that lowers every instance in an ordinary build. The instances of one specialization are
  one by their name, so lowering each again is the opposite of reuse (invariant 2).
