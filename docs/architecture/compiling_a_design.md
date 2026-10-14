# Compiling A Design

Three parts of this are not realized: the design root does not yet state which generate blocks each
instance built (a body still holds every alternative any of its instances built and decides among
them as it constructs), an ordinary build still lowers every block of a loop and every instance
handed new values and compares them, and the key is not yet read off the lowering's own reads
everywhere. The contract binds every layer regardless.

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
// fix what the element declares.
class Leaf : public Scope { public: Logic<8> in, out; int K; Scope* g; };

// One body per way the values fix its statements; compiled once, shared.
class LeafBody final : public Leaf {
  LeafBody(const Record& r) { K = r.K; g = r.g(this); }   // builds what r says, decides nothing
};
class G0 ...  class G1 ...                                // each generate block: a definition of its own

// The design root: data, one record per instance, equal subtrees stored once.
row[0].u: { make: LeafBody, K: 0, g: G0::Make }
row[1].u: { make: LeafBody, K: 1, g: G1::Make }
```

Three kinds of thing, and nothing else:

| Kind              | What it is                                                         | Count follows                            |
| ----------------- | ------------------------------------------------------------------ | ---------------------------------------- |
| Signature         | what referrers compile against: what the element declares          | distinct values of what declarations use |
| Body              | the code: processes, subroutines, how to build its children        | distinct values of what statements use   |
| Construction data | per instance: values, which body each child is, which blocks stand | instances (data, never code)             |

A value only statements use names a body and leaves the signature alone, because a child instance is
a member of its parent's signature -- a hierarchical name reaches into it -- so a value in the
child's signature would be in the parent's, and in every signature above it. The parent holds the
child by its signature, and the body the object is answers each call, as a virtual call does in C++
and a call through `dyn Trait` does in Rust.

A value parameter is an argument of the class only where something the class states uses it -- a
width, a type, a constant a statement requires. Everywhere else it is a field the object is built
with, read at run time, so a loop handing each child its index makes one class and one body, not one
per index.

## The decisions

**An artifact's name is decided from inputs, before it is built.** A referrer names the class it
compiles against without opening the body, and both sides derive the name by one function of the
same inputs, so units compile in any order and in parallel. The name is the definition plus the
values of the parameters it uses -- those its declarations rely on for the signature, those only its
statements rely on for a body. The output of a lowering is never its identity.

**Which parameters a name uses is what the lowering reads, recorded where it was read.** The front
end records which parameters each constant it was required to evaluate read, and which ones settled
a type it reads as a fact; the lowering reads an instance's tree only through the expression as
written, the types declarations state, and those recorded constants and types, and works out
everything else itself. So two instances of one name lower alike by construction, and the key is the
whole of what the lowering could see. A read the key missed is the compiler's defect, found by the
tests that lower every instance and compare; a build never compares.

**Which blocks an instance built is construction data, not code.** The front end has already decided
every generate condition and counted every loop for every instance. Stating those answers per
instance keeps a body a function of its name alone: an instance added elsewhere, selecting another
alternative, changes data and compiles one new block, never an existing body or header. A body
holding every alternative its instances built would make its content follow instances it is not
named for.

**A body is not re-decided at construction.** It builds the children its record lists, with the
values the record holds. Nothing the front end already settled is evaluated again at time zero.

**The content of an artifact decides only whether its dependents rebuild.** A rebuilt body whose
output is unchanged stops the rebuild there; equal code under two names may be folded at the link,
which renames nothing.

## How the scheme lines up

| This scheme                                          | rustc                                                                                                 | C++                                                   | Bazel / ccache                                    |
| ---------------------------------------------------- | ----------------------------------------------------------------------------------------------------- | ----------------------------------------------------- | ------------------------------------------------- |
| Name = definition + values of the parameters it uses | `Instance { def, args }`                                                                              | template instantiation by arguments                   | action key = digest of its inputs                 |
| Uses recorded as the lowering reads them             | a query's reads are its dependency edges                                                              | --                                                    | discovered inputs (`.d` files), ccache's manifest |
| A value read at run time, not in the name            | the proposed successor to polymorphization: monomorphic data, polymorphic code with a hidden argument | a constructor argument                                | --                                                |
| Signature / body                                     | `type_of` and `fn_sig` / `optimized_mir`, queried apart so a body edit reaches no caller              | header / translation unit; module interface           | an action's declared outputs                      |
| Construction data in the design root                 | --                                                                                                    | the arguments `main` builds objects with              | --                                                |
| Output content stops a rebuild                       | red/green: an equal result fingerprint is green                                                       | a byte-identical module interface (update-if-changed) | output digest is the next action's input          |
| Equal code folded without renaming                   | LLVM MergeFunctions                                                                                   | linker identical-code folding                         | --                                                |
| Every instance lowered and compared, in tests only   | `-Zincremental-verify-ich`                                                                            | --                                                    | --                                                |

Where the conditions differ: a template argument is written at type level, while a parameter's value
follows the instance (a loop hands each child its index) -- so a name holds the values of the
parameters the class uses, and one only read as a value is a constructor argument. And the front end
elaborates every instance into a folded tree of its own, so what a body depends on is read off the
reads the lowering makes, never predicted from the text.

## Models this one replaces

| Model                                                | For                                      | Against                                                                                     |
| ---------------------------------------------------- | ---------------------------------------- | ------------------------------------------------------------------------------------------- |
| An artifact per instance                             | trivially correct                        | artifacts follow instances; every instance compiled                                         |
| A name holding every parameter's whole value         | the field's answer; nothing to classify  | a value following instances splits per instance                                             |
| Lower every instance, compare, share the equal       | correct without classifying anything     | the name waits on the body; cost grows with instances, comparisons with distinct bodies     |
| A name from a hash of the lowered body               | no classifier; a miss costs only sharing | the name is known only after building; the field never does this; hashes merge unequal code |
| A name from an analysis predicting what a body reads | the name before the body                 | the analysis misses reads; rustc removed its version of it                                  |
| A body holding every block its instances built       | decides at construction like the source  | the body follows instances it is not named for; re-evaluates what the front end settled     |

## Core Invariants

1. An artifact's name is a function of its definition and of the values of the parameters it uses,
   and of nothing produced by building it.
2. A lowering reads of an instance only what its name holds, so every instance of one name lowers to
   the same artifact.
3. Whatever the front end decided per instance and the name does not hold -- a value read as a
   value, which blocks stand, which body each child is -- reaches the object as construction data.
4. The count of artifacts follows distinct names; only construction data follows instances.
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
