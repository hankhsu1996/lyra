# Compiling A Design

## Purpose

A design is compiled to as few artifacts as the source and its values require, each named before it
is built and built from nothing its name does not hold, so that an edit rebuilds what it changed and
nothing else. This doc is the whole of that scheme in one place: what an artifact is, what decides
its name, what flows in at run time instead, and how the scheme lines up with the compilers and
build systems it is modelled on. The parts each have a contract of their own --
`specialization_model.md` for what an argument is, `incremental_build.md` for keys, fingerprints and
the staging, `emission_model.md` for what an artifact contains -- and this doc restates none of
them.

## The model

Read a design element as a C++ class template whose parameters a design may hand values, and read
the design as a program that builds objects of it:

```cpp
// The signature every referrer compiles against: one header per way the values
// fix what the element declares and per set of generate blocks it built.
class Leaf_odd : public Scope { public: Logic<8> in, out; int K; struct odd; Scope* g; };

// One body per way the values fix its statements; compiled once, shared.
class LeafBody final : public Leaf_odd {
  LeafBody(int K) : K(K) { if (K % 2 == 1) g = new odd(this); }   // runs the source's generate
};

// The design root: which body each instance is, and what it is handed.
u0: { make: LeafBody, K: 1 }
u1: { make: LeafBody, K: 3 }
```

Three kinds of thing, and nothing else:

| Kind              | What it is                                                  | Count follows                                                           |
| ----------------- | ----------------------------------------------------------- | ----------------------------------------------------------------------- |
| Signature         | what referrers compile against: what the element declares   | distinct values of what declarations use, and the generate blocks built |
| Body              | the code: processes, subroutines, how to build its children | distinct values of what statements use                                  |
| Construction data | per instance: the values it is handed                       | instances (data, never code)                                            |

A value only statements use names a body and leaves the signature alone, because a child instance is
a member of its parent's signature -- a hierarchical name reaches into it -- so a value in the
child's signature would be in the parent's, and in every signature above it. The parent holds the
child by its signature, and the body the object is answers each call, as a virtual call does in C++
and a call through `dyn Trait` does in Rust.

A value parameter is an argument of the class only where something the class states uses it -- a
width, a type, a constant a statement requires. Everywhere else it is a field the object is built
with, read at run time, so a loop handing each child its index makes one class and one body, not one
per index. The one exception is an index that decides which generate blocks the child builds. The
child is then one unit per set of blocks its instances built, however many indices select each set;
but the loop's block declares the child as a member of the child's unit, so the index names the
block's class, one per index value, as `Leaf<i>` names one class per `i` in C++.

## The decisions

**An artifact's name is decided from inputs, before it is built.** A referrer names the class it
compiles against without opening the body, and both sides derive the name by one function of the
same inputs, so units compile in any order and in parallel. The name is the definition, the values
of the parameters it uses -- those its declarations rely on for the signature, those only its
statements rely on for a body -- and the generate blocks it built. The output of a lowering is never
its identity.

**Which parameters a name uses is what the lowering reads, recorded where it was read.** The front
end records which parameters each constant it was required to evaluate read, and which ones settled
a type it reads as a fact; the lowering reads an instance's tree only through the expression as
written, the types declarations state, and those recorded constants and types, and works out
everything else itself. So two instances of one name lower alike by construction, and the key is the
whole of what the lowering could see. A read the key missed is the compiler's defect, found by the
tests that lower every instance, and every part of a body from another block that built it, and
compare; a build never compares.

**Which generate blocks an instance built is part of its name.** An alternative a conditional did
not select is not elaborated and need not be valid for the instance's values (LRM 27.5), so it
cannot be compiled from an instance that did not select it. What was built is therefore part of what
an instance is, as the branch an `if constexpr` keeps is part of a C++ instantiation. The name holds
which blocks were built and not the value that selected them, so two values selecting one
alternative are one body. How many blocks a loop counts out is not part of it. Within one instance
the blocks of one name are one body, lowered once: each of its parts from one block that built it,
and what the blocks counted out and selected gathered from all of them.

**The constructor runs the source's generate.** Its conditions and loops are constructor code, run
against the values the object was handed, as a C++ constructor runs its own; the name guarantees
that whatever they select is among the blocks the body holds.

**The content of an artifact decides only whether its dependents rebuild.** A rebuilt body whose
output is unchanged stops the rebuild there; equal code under two names may be folded at the link,
which renames nothing.

## How the scheme lines up

| This scheme                                                                                 | rustc                                                                                                 | C++                                                   | Bazel / ccache                                    |
| ------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- | ----------------------------------------------------- | ------------------------------------------------- |
| Name = definition + values of the parameters it uses                                        | `Instance { def, args }`                                                                              | template instantiation by arguments                   | action key = digest of its inputs                 |
| Uses recorded as the lowering reads them                                                    | a query's reads are its dependency edges                                                              | --                                                    | discovered inputs (`.d` files), ccache's manifest |
| A value read at run time, not in the name                                                   | the proposed successor to polymorphization: monomorphic data, polymorphic code with a hidden argument | a constructor argument                                | --                                                |
| Signature / body                                                                            | `type_of` and `fn_sig` / `optimized_mir`, queried apart so a body edit reaches no caller              | header / translation unit; module interface           | an action's declared outputs                      |
| Generate blocks built, in the name                                                          | the arguments an `Instance` is monomorphized with                                                     | the branch an `if constexpr` keeps                    | --                                                |
| Values handed in at construction                                                            | --                                                                                                    | the arguments `main` builds objects with              | --                                                |
| Output content stops a rebuild                                                              | red/green: an equal result fingerprint is green                                                       | a byte-identical module interface (update-if-changed) | output digest is the next action's input          |
| Equal code folded without renaming                                                          | LLVM MergeFunctions                                                                                   | linker identical-code folding                         | --                                                |
| Every instance, and every body part from another block, lowered and compared, in tests only | `-Zincremental-verify-ich`                                                                            | --                                                    | --                                                |

Where the conditions differ: a template argument is written at type level, while a parameter's value
follows the instance (a loop hands each child its index) -- so a name holds the values of the
parameters the class uses, and one only read as a value is a constructor argument. The front end
elaborates every instance into a folded tree of its own, so what a body depends on is read off the
reads the lowering makes, never predicted from the text. And the generate blocks in a name are read
off the front end's elaboration of each instance, where rustc names an `Instance` by its definition
and arguments alone. What an instance built is a function of its definition and the values its
generate scheme read, which the front end records, so this reads the result of evaluating the
instance's own scheme, as a constant it evaluated is read; it never crosses into an instance the
instance holds, whose blocks are that instance's own name. The cost is that a name cannot be worked
out until the front end has elaborated the instance.

## Models this one replaces

| Model                                                       | For                                      | Against                                                                                                                        |
| ----------------------------------------------------------- | ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| An artifact per instance                                    | trivially correct                        | artifacts follow instances; every instance compiled                                                                            |
| A name holding every parameter's whole value                | the field's answer; nothing to classify  | a value following instances splits per instance                                                                                |
| Lower every instance, compare, share the equal              | correct without classifying anything     | the name waits on the body; cost grows with instances, comparisons with distinct bodies                                        |
| A name from a hash of the lowered body                      | no classifier; a miss costs only sharing | the name is known only after building; the field never does this; hashes merge unequal code                                    |
| A name from an analysis predicting what a body reads        | the name before the body                 | the analysis misses reads; rustc removed its version of it                                                                     |
| A body holding every block its instances built              | one body whatever its instances built    | the body follows instances it is not named for; every instance handed new values is merged                                     |
| Each block its own artifact, chosen by data the root states | one body whatever its instances built    | no compiler shares code across instantiations whose kept branches differ; every name a block reads in its module crosses units |

## Core Invariants

1. An artifact's name is a function of its definition, of the values of the parameters it uses and
   of the generate blocks elaboration built, and of nothing produced by building it.
2. A lowering reads of an instance only what its name holds, so every instance of one name lowers to
   the same artifact.
3. Which generate blocks an instance built is part of its name; how many blocks a loop counted out
   is not, and a value read as a value reaches the object when it is built.
4. The count of artifacts follows distinct names; only construction data follows instances. Where a
   value a scope hands a child decides which generate blocks the child builds, the value names the
   scope's class, so a loop handing each child its index for that purpose has a block class per
   index value and its classes follow the loop's count.
5. A build decides nothing by comparing artifacts; comparison checks the scheme, in tests.

## Forbidden Shapes

- A name, or which artifact an instance is, decided by comparing or hashing what was lowered. It
  makes the name wait on the body, which breaks compiling referrers first (invariant 1).
- A lowering reading a fact the front end folded opportunistically -- a range it handed up, a branch
  it left dead, a read its analysis dropped. Two instances of one name then lower differently
  (invariant 2).
- A body whose content depends on instances it is not named for, such as one holding what any of
  them built. Adding an instance elsewhere then changes it (invariant 3).
- A parameter in a name where it is only read as a value. A value that follows the instance then
  makes artifacts follow instances (invariant 4).
- A check that lowers every instance in an ordinary build. It is the opposite of reuse (invariant
  5).
