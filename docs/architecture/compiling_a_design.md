# Compiling A Design

## Purpose

A design is compiled to as few artifacts as the source and its values require, what referrers
compile against named before anything is built and from nothing building produces, so that an edit
rebuilds what it changed and nothing else. This doc is the whole of that scheme in one place: what
an artifact is, what decides its name, what flows in at run time instead, and how the scheme lines
up with the compilers and build systems it is modelled on. The parts each have a contract of their
own -- `specialization_model.md` for what an argument is, `incremental_build.md` for keys,
fingerprints and the staging, `emission_model.md` for what an artifact contains -- and this doc
restates none of them.

## The model

Read a design element as a C++ class template whose parameters a design may hand values, and read
the design as a program that builds objects of it:

```cpp
// The signature every referrer compiles against: one header per way the values
// fix what the element declares and per set of generate blocks it built.
class Leaf_odd : public Scope { public: Logic<8> in, out; int K; struct odd; Scope* g; };

// One body per distinct code its specializations come out as; compiled once, shared.
class LeafBody final : public Leaf_odd {
  LeafBody(int K, const Table* t) : K(K) {   // runs the source's generate
    if (K % 2 == 1) g = t->block(0, 0)(this);
  }
};

// The design root, per specialization: which body it is and which body each
// child and block it builds is; K is handed by the parent's code.
u0: { make: LeafBody, blocks: { odd: LeafBody::create_block_0 } }
u1: { make: LeafBody, blocks: { odd: LeafBody::create_block_0 } }
```

Three kinds of thing, and nothing else:

| Kind              | What it is                                                                                      | Count follows                                                           |
| ----------------- | ----------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------- |
| Signature         | what referrers compile against: what the element declares                                       | distinct values of what declarations use, and the generate blocks built |
| Body              | the code: processes, subroutines, how to build its children                                     | distinct code its specializations come out as                           |
| Construction data | per specialization: which body each child and generate block is, and the table it is built with | specializations and the blocks they build (data, never code)            |

A value only statements use names a specialization and leaves the signature alone, because a child
instance is a member of its parent's signature -- a hierarchical name reaches into it -- so a value
in the child's signature would be in the parent's, and in every signature above it. The parent holds
the child by its signature, and the body the object is answers each call, as a virtual call does in
C++ and a call through `dyn Trait` does in Rust.

A value parameter is an argument of the class only where something the class declares uses it -- a
width, a type, a constant a declaration requires. Everywhere else it is a field the object is built
with, read at run time, so a loop handing each child its index makes one class and, where the code
comes out alike, one body, not one per index. The one exception is an index that decides which
generate blocks the child builds. The child is then one unit per set of blocks its instances built,
however many indices select each set; but the loop's block declares the child as a member of the
child's unit, so the index names the block's class, one per index value, as `Leaf<i>` names one
class per `i` in C++.

## The decisions

**What referrers compile against is named from inputs, before anything is built.** A referrer names
the class it compiles against without opening a body, and both sides derive the name by one function
of the same inputs, so units compile in any order and in parallel. The name is the definition, the
values of the parameters its declarations rely on, and the generate blocks it built. The output of a
lowering is never the identity a referrer reads.

**Which parameters a name uses is what the lowering reads, recorded where it was read.** The front
end records which parameters each constant it was required to evaluate read, and which ones settled
a type it reads as a fact; the lowering reads an instance's tree only through the expression as
written, the types declarations state, and those recorded constants and types, and works out
everything else itself. So two instances of one specialization lower alike by construction, and the
key is the whole of what the lowering could see. A read the key missed is the compiler's defect,
found by the check every compile makes that the specializations of one unit publish alike, and by
the tests that lower every instance and compare.

**Which generate blocks an instance built is part of its name.** An alternative a conditional did
not select is not elaborated and need not be valid for the instance's values (LRM 27.5), so it
cannot be compiled from an instance that did not select it. What was built is therefore part of what
an instance is, as the branch an `if constexpr` keeps is part of a C++ instantiation. The name holds
which blocks were built and not the value that selected them. How many blocks a loop counts out is
not part of it.

**A body is found by comparing what each specialization came out as.** Every value an instance is
handed names its specialization, whose objects are all built alike. Lowering a specialization to HIR
is cheap beside everything after it, so each is lowered, and specializations of one unit whose HIR
is equal are one body -- a digest narrows the comparison, which is whole, as a linker folds
identical sections. The body is named for the first specialization in the design's order that came
out as it, and only that one is compiled further. Every generate block is lowered on its own and is
a body of its own, shared with every block of the same body that comes out alike; which body stands
at each place is the object's construction.

**The constructor runs the source's generate.** Its conditions and loops are constructor code, run
against the values the object was handed, as a C++ constructor runs its own; each block it builds is
made by the entry its construction table holds at that place.

**The content of an artifact decides only whether its dependents rebuild.** A rebuilt body whose
output is unchanged stops the rebuild there.

## How the scheme lines up

| This scheme                                                       | rustc                                                                                                 | C++                                                   | Bazel / ccache                                    |
| ----------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- | ----------------------------------------------------- | ------------------------------------------------- |
| Name = definition + values of the parameters its declarations use | `Instance { def, args }`                                                                              | template instantiation by arguments                   | action key = digest of its inputs                 |
| Uses read off what the lowering produced                          | a query's reads are its dependency edges                                                              | --                                                    | discovered inputs (`.d` files), ccache's manifest |
| A value read at run time, not in the name                         | the proposed successor to polymorphization: monomorphic data, polymorphic code with a hidden argument | a constructor argument                                | --                                                |
| Signature / body                                                  | `type_of` and `fn_sig` / `optimized_mir`, queried apart so a body edit reaches no caller              | header / translation unit; module interface           | an action's declared outputs                      |
| Generate blocks built, in the name                                | the arguments an `Instance` is monomorphized with                                                     | the branch an `if constexpr` keeps                    | --                                                |
| Values handed in at construction                                  | --                                                                                                    | the arguments `main` builds objects with              | --                                                |
| Equal code shared as one body                                     | LLVM MergeFunctions                                                                                   | linker identical-code folding                         | --                                                |
| Output content stops a rebuild                                    | red/green: an equal result fingerprint is green                                                       | a byte-identical module interface (update-if-changed) | output digest is the next action's input          |
| Every instance lowered and compared, in tests only                | `-Zincremental-verify-ich`                                                                            | --                                                    | --                                                |

Where the conditions differ: a template argument is written at type level, while a parameter's value
follows the instance (a loop hands each child its index) -- so a name holds the values of the
parameters the class declares with, and one only read as a value is a constructor argument. The
front end elaborates every instance into a folded tree of its own, so what a declaration depends on
is read off the reads the lowering makes, never predicted from the text. Equal code is found before
anything is compiled rather than at the link, because lowering to HIR is cheap and everything after
it is not, and because which body each child is, is construction data rather than code, so a body
compares without its children. And the generate blocks in a name are read off the front end's
elaboration of each instance, where rustc names an `Instance` by its definition and arguments alone.
What an instance built is a function of its definition and the values its generate scheme read,
which the front end records, so this reads the result of evaluating the instance's own scheme, as a
constant it evaluated is read; it never crosses into an instance the instance holds, whose blocks
are that instance's own name. The cost is that a name cannot be worked out until the front end has
elaborated the instance.

## Models this one replaces

| Model                                          | For                                      | Against                                                                                               |
| ---------------------------------------------- | ---------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| An artifact per instance                       | trivially correct                        | artifacts follow instances; every instance compiled                                                   |
| A name holding every parameter's whole value   | the field's answer; nothing to classify  | a value following instances splits per instance                                                       |
| A unit's name from comparing what was lowered  | correct without classifying anything     | what referrers compile against waits on the bodies; cost grows with instances                         |
| A name from a hash of the lowered body         | no classifier; a miss costs only sharing | the name is known only after building, and every edit of a body renames it; hashes merge unequal code |
| A body named by the parameters its code reads  | the body before it is lowered            | a prediction: two values settling one constant are two bodies, and how an object builds has no owner  |
| A body holding every block its instances built | one body whatever its instances built    | the body follows instances it is not named for; every instance handed new values is merged            |

## Core Invariants

1. What a referrer compiles against -- a unit's name -- is a function of its definition, of the
   values of the parameters its declarations use and of the generate blocks elaboration built, and
   of nothing produced by building it.
2. A lowering reads of an instance only what its specialization's name holds, so every instance of
   one specialization lowers to the same code and builds the same children.
3. Which generate blocks an instance built is part of its unit's name; how many blocks a loop
   counted out is not, and a value read as a value reaches the object when it is built.
4. The count of code artifacts follows distinct bodies; construction data follows specializations
   and the blocks they build. Where a value a scope hands a child decides which generate blocks the
   child builds, the value names the scope's class, so a loop handing each child its index for that
   purpose has a block class per index value and its classes follow the loop's count.
5. Which body a specialization is, is found by comparing its HIR whole, a digest only narrowing the
   comparison, and nothing but the design's construction data reads the answer.

## Forbidden Shapes

- A unit's name decided by comparing or hashing what was lowered. It makes what referrers compile
  against wait on the bodies, which breaks compiling referrers first (invariant 1).
- A lowering reading a fact the front end folded opportunistically -- a range it handed up, a branch
  it left dead, a read its analysis dropped. Two instances of one specialization then lower
  differently (invariant 2).
- A body whose content depends on instances it is not named for, such as one holding what any of
  them built. Adding an instance elsewhere then changes it (invariant 3).
- A parameter in a unit's name where it is only read as a value. A value that follows the instance
  then makes signatures follow instances (invariant 4).
- Two bodies taken as one on a digest alone. Bodies that differ become one wherever the digest
  misses a field or collides (invariant 5).
- A parent's code naming which body a child or a block is. Specializations sharing that code may
  build different ones (invariant 5).
- A check that lowers every instance in an ordinary build. The instances of one specialization are
  one by their name, so lowering each again is the opposite of reuse (invariant 2).
