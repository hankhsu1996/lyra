# A Unit Is Compiled To One Body Per Distinct Code

## Date

2026-10-13

## Status

Accepted. Revises
[a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
D1 and D4, and extends
[a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D5 from a
block to a unit. D3 revised 2026-10-14 by
[a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md):
every body declares what the unit publishes from its own instance, since the generate blocks built
are part of the unit's name.

Revised 2026-10-25, with
[a-body-states-nothing-of-another-bodys-implementation](a-body-states-nothing-of-another-bodys-implementation.md):
a body is found from what a specialization's code came out as, not predicted from which parameters
the code reads. The 2026-10-13 shape named each body by the values of the parameters its code read
(D1, D5), which was a prediction of what the code would be: it compiled two values that settle one
constant twice, and it had a body follow every value a child's code read, because which body a child
was had to be part of the parent's code. Two facts it rested on too few of are now stated (facts 8
and 9): how an object builds its children differs between values its code does not read, and the HIR
of a specialization is cheap beside everything after it. So every value an instance is handed names
its specialization, as a template argument names an instance in C++ (fact 2 stands), and
specializations whose code comes out alike are one body, as a linker folds identical code.

D1, D4 and D5 revised 2026-10-28 by
[a-unit-is-the-specializations-that-publish-alike](a-unit-is-the-specializations-that-publish-alike.md):
a unit is the specializations whose published structure reads alike, found by comparing what each
publishes, so no parameter's value is predicted to name it; a design element declaring a class is a
class per specialization, for the reason that record's D5 gives; and a value handed to a child names
nothing of the scope handing it, whose published member already names the child's class; and the
design root's tables are stated by what a specialization is built as, not by the specialization
(that record's D9). D2, D3 and D6 stand.

## Requirement

A unit is compiled as one class every referrer compiles against, named by the values of the
parameters what its instances declare relies on; each set of values its instances are handed names a
specialization, whose objects are all built alike; and each distinct code its specializations come
out as is one body, which compiles on its own, so adding or editing one recompiles nothing another
body or a referrer compiled.

The program it stands for, written by hand:

```cpp
// Child.hpp -- what every referrer includes, named by the values the declarations rely on
class Child : public Scope {
 public:
  Logic<8> r;
  int P;                    // read as a value: the object's own
  virtual int f(int) = 0;   // a referrer calls through the object
  Child(...);               // defined inline below: every body states it
};

// Child__a.cpp -- one body: the code the specializations P=1 and P=3 both come out as
class Impl final : public Child { void comb() { r = in.bits(1, 0); }  int f(int) override; };
std::unique_ptr<Child> Make(...);

// the design root: per specialization, the entry of the body it is and how it builds its children
table(Child P=1) = { entry: Child__a::Make, ... };
table(Child P=3) = { entry: Child__a::Make, ... };
```

## Facts it rests on

1. A parameter is a constant fixed before the run (LRM 6.20, 23.10), and nothing requires the
   compiled class to hold its value; where something compiled relies on it -- a constant the
   language requires, a type read as a fact -- the front end resolves it in each elaboration, and a
   parameter read anywhere else is an expression the lowering can read at run time.
2. A parameter's values follow its instances -- a loop hands each child its index. A parameter read
   only as a value is handed at construction, so the artifact count does not follow them there;
   where something compiled relies on one, an instance per value is what a C++ template and a rustc
   `Instance` make too, and two values that settle one constant (`i % 2`, `$clog2(DEPTH)`) are two
   instances (the human, 2026-10-13).
3. A generate block of a loop is already named by what it declares, as resolved, and its body is
   found by comparing what each block came out as.
4. What a unit publishes is what every one of its instances built, since a name reaching into a
   block has to find it whichever instance built it.
5. A class the source declares in a scope reaches that scope's object as its body compiled it -- a
   name a method writes is a route slot only the body holds -- while it is one class of the unit.
6. A child's specialization may follow a value its parent was handed (a parent handing its parameter
   on to a child), and which specialization each instantiation makes is how the parent's object is
   built, which its code does not state.
7. clang gives a class with no key function `linkonce_odr` in every translation unit using it, and a
   class of a named module only in the module's own unit (`CodeGenModule::getVTableLinkage`); rustc
   instantiates most items once with external linkage and copies only small inlinable ones into each
   codegen unit (`MonoItem::instantiation_mode`).
8. Which blocks an object builds, how many a loop counts out, and which specialization each child is
   follow every value the object is handed, including those its code does not read and only its
   construction runs on, which makes each such value an input of how the object is built (run,
   2026-10-25: two instances of one module handed `S = 0` and `S = 1` build four and three blocks of
   one loop).
9. Lowering a specialization to HIR is cheap beside everything after it (HIR of 64 blocks in 17 ms,
   everything after it 5.7 s, 2026-10-25), and lld's identical code folding and LLVM's
   `MergeFunctions` fold equal code after it is made by comparing it, a digest only narrowing what
   is compared.

## Decision

**D1. A unit is named by the values of the parameters what it declares relies on, and a
specialization of it by every value its instances are handed.** The unit's name holds the values of
its arguments, as a block's name holds what it declares (fact 3); a parameter a nested design
element reads, one whose type is its value's, one a class reads, and a top instance's are arguments
of the unit. The specialization's name is the unit's with a digest of every value an instance is
handed at construction, and is the unit's own where there is none. Instances of one specialization
are built alike (fact 8), so it is what the design's construction tables are stated by.

**D2. A parameter read as a value is read as what the instance was built with.** Unless it is an
argument of the unit, a supplied parameter is handed at construction and every read of it reads the
object's own.

**D3. A body is the code a specialization comes out as, compiled on its own.** Each specialization
is lowered from the instance it is named for into HIR, and declares the unit's signature from that
instance, and a later specialization of the unit adds the class of each generate block only it built
([a-unit-is-the-specializations-that-publish-alike](a-unit-is-the-specializations-that-publish-alike.md)
D4, D8), since which blocks were built is no part of a unit. What it states beside how its objects
are built is looked up among what the specializations before it came out as, in the design's order:
a digest of its text narrows which are compared, and it shares one only where everything they state
is equal (fact 9). One that shares is no further work; one that does not is a body, named for that
specialization, and goes on to be compiled. The published classes are built from declarations alone,
so every body states them alike, and the link keeps one of each (fact 7): the execution backend
links them weak, and the C++ backend defines them inline in the header every body writes alike,
which the project keeps one copy of. What only a body states -- the classes realizing its scopes,
its closures, the functions nothing else names -- is named by the body: under its name on the
execution backend, and in the namespace with no name on the C++ backend.

**D4. A scope declaring a class has one body per value its code relies on.** A class reaches the
object of the scope declaring it as that scope's body compiled it (fact 5), so a parameter the code
of a scope that declares a class relies on, anywhere inside it, is an argument of the scope's name
instead. This holds for a unit and for a generate block alike.

**D5. A parameter handed to a child's argument is an argument of the scope building the child where
it names the child's unit.** One handed to an argument of what the child declares is an argument of
what the building scope declares, since that scope declares a member of the child's class. One
handed to a value the child is handed at construction names the child's specialization, which is how
the building scope's object is built and no part of its code (fact 6).

**D6. The design root makes each instance from the entry of the body its specialization is.** An
instantiation's construction states the specialization it makes; the table it hands on is that
specialization's, and the entry making an object of it is the entry of the body it came out as,
named under the body. Every body's entry of a unit is declared with the one prototype the unit
published, so a table names it at that type and erases it by a conversion it states, as it does an
entry of its own.

## Limits, each with its cause

- Two values that settle every constant alike are two specializations, each with a table of its own,
  and one body (D1, D3).
- One copy of each distinct body of the unit being lowered is held, since two are one only where
  everything they state is equal; the specializations of one unit are lowered one after another, so
  no more than one unit's are held at a time (D3; fact 9).
- A body is named for the first specialization, in the design's order, that came out as it, so an
  edit that changes which specialization is first renames it, and what named it is compiled again
  (D3).
- A unit or a block declaring a class is a unit per value its code relies on (D4).
- Every body compiles the published classes' constructor, destructor and definition again; the link
  keeps one (D3).
- An automatic local whose width a supplied value fixes is counted as declared, because the front
  end records the scope an evaluation was made in and not the declaration it was made for.

## Survey

Where a unit's body is compiled: C++ instantiates a template where it is used, and an explicit
instantiation (`template class X<3>;`, clang `Sema::ActOnExplicitInstantiation`) compiles it beside
the definition, in one translation unit or several; rustc makes each instance an item of the
monomorphization collector. Both name an instance by its whole arguments, and so does a
specialization here. The condition that differs is which parameters are arguments: C++ and Rust make
the author write a template argument apart from a constructor argument, and SystemVerilog writes
both as `parameter`, so the arguments of the unit are the parameters what it declares relies on.

Where equal code is shared: C++ and Rust compile an instance per argument set and leave folding to
the linker (lld `--icf`, which partitions sections by a hash and then compares them) or to LLVM's
`MergeFunctions` pass, which compares functions after hashing them; CIRCT's `Dedup` merges equal
modules bottom-up, and merged unequal ones where it trusted a hash alone (llvm/circt #7415). The
condition that differs is where the comparison runs: here before anything is compiled, on HIR,
because lowering to HIR is cheap and everything after it is not (fact 9), and because which body
each child is, is construction data rather than code, so a body compares without its children.

Where what every body states is compiled: clang's header-defined class is stated by every
translation unit and kept once by the linker, and a named module's class only by its own unit (fact
7). A unit of its own for the published classes would hold three small definitions and cost a
compilation per module whether or not the unit has a second body, against the edit-compile-run loop;
what every body restates instead is what a header-defined class restates.

## Rejected

- **A body named by the values of the parameters its code reads (this record's 2026-10-13 D1).** A
  prediction of what the code would be: two values that settle one constant were two bodies, a
  parent was a body per value its child's code read, and how an object builds its children, which
  follows every value (fact 8), had no specialization of its own to be stated by.
- **A name holding the constants settled from a parameter at each place, not its value.** It shared
  values that fold alike, and needed the front end to say which constant at which place every fact
  came from; a type settled from a fold the front end made only because it could is a fact it cannot
  place (fact 2).
- **A body named by a digest of what it states.** Every edit of a body renames it, so the design
  root's tables, which name it, are compiled again, and an emitted file is named by a digest.
- **A body taken equal to another by a digest alone.** Two bodies that differ would be one wherever
  the digest misses a field or collides; the comparison is whole, as the linkers' is (fact 9).
- **A unit of its own holding what the unit published.** A compilation per module for three small
  definitions, and a class the source declares in the unit could not live there (fact 5).
- **Every body of a unit in one compilation.** Adding an instance with a new value would recompile
  every body, and bodies would not compile in parallel.
- **A class's route slots published, so a class reaches only the published part.** The published
  layout would follow a class method's body, and a method edit would rewrite every referrer's
  header.

## Revises

- [a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
  D1: a parameter something declared relies on enters the unit's name by its value, and every value
  an instance is handed enters its specialization's. D4: a parameter is read as its settled value
  only where it is an argument of the unit.
- [a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D5 and D7: a
  unit is realized as a block is, one level out, and a scope declaring a class has one body.
