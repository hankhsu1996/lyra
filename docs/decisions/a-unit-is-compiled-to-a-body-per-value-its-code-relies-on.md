# A Unit Is Compiled To A Body Per Value Its Code Relies On

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

## Requirement

A unit is compiled as one class every referrer compiles against, named by the values of the
parameters what its instances declare relies on, and one body for each set of values of the
parameters only what its code runs relies on; each body compiles on its own, and adding or editing
one recompiles nothing another body or a referrer compiled.

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

// Child.body_a.cpp -- one body, named by the value of P its code relies on
class Impl final : public Child { void comb() { r = in.bits(1, 0); }  int f(int) override; };
std::unique_ptr<Child> Make(...);

// the design root: the entry of the body each instantiation makes
table(Top) = { a: Child_body_a::Make, b: Child_body_b::Make };
```

## Facts it rests on

1. A parameter is a constant fixed before the run (LRM 6.20, 23.10), and nothing requires the
   compiled class to hold its value; where something compiled relies on it -- a constant the
   language requires, a type read as a fact -- the front end records which parameters it read.
2. A parameter's values follow its instances -- a loop hands each child its index. A parameter read
   only as a value is handed at construction, so the artifact count does not follow them there;
   where something compiled relies on one, a body per value is what a C++ template and a rustc
   `Instance` make too, and two values that settle one constant (`i % 2`, `$clog2(DEPTH)`) are two
   bodies (the human, 2026-10-13).
3. A generate block is already named by the value of its index where what it declares relies on it,
   and realized once per value its code relies on, each realization naming the published class it
   extends.
4. What a unit publishes is what every one of its instances built, since a name reaching into a
   block has to find it whichever instance built it.
5. A class the source declares in a scope reaches that scope's object as its body compiled it -- a
   name a method writes is a route slot only the body holds -- while it is one class of the unit.
6. A child's body may follow a value its parent was handed (a parent handing its parameter on to a
   child whose code alone reads it), and a construction table is stated per body.
7. clang gives a class with no key function `linkonce_odr` in every translation unit using it, and a
   class of a named module only in the module's own unit (`CodeGenModule::getVTableLinkage`); rustc
   instantiates most items once with external linkage and copies only small inlinable ones into each
   codegen unit (`MonoItem::instantiation_mode`).

## Decision

**D1. A unit is named by the values of the parameters what it declares relies on, and each body of
it by those of the parameters only its code relies on.** The front end's record says which
parameters something compiled relied on, and where: in an expression a statement of a process, a
continuous assignment or a subroutine holds, they are the body's arguments; anywhere else, the
unit's. The unit's name holds the values of its arguments, as a block's name holds its index's (fact
3); the body's name is the unit's with a digest of the values of the body's arguments, and is the
unit's own where there are none. A parameter a nested design element reads, one whose type is its
value's, one a class reads, and a top instance's are arguments of the unit.

**D2. A parameter read as a value is read as what the instance was built with.** Unless it is an
argument of the unit, a supplied parameter is handed at construction and every read of it reads the
object's own. An argument of a body only is handed too: the body's name already holds its value, so
reading the object's own costs nothing and keeps one way of reading a supplied parameter.

**D3. A body is a compilation of its own, and states what the unit publishes alike.** Each body is
lowered from the instance it is named for into a unit of its own, named by the body's name, and
declares the unit's signature from that instance: which generate blocks were built is part of the
unit's name
([a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)),
so every body of a unit built the same ones. The published classes are built from declarations
alone, so every body states them alike, and the link keeps one of each (fact 7): the execution
backend links them weak, and the C++ backend defines them inline in the header every body writes
alike, which the project keeps one copy of. What only a body states -- the classes realizing its
scopes, its closures, the functions nothing else names -- is named by the body: under its name on
the execution backend, and in the namespace with no name on the C++ backend.

**D4. A scope declaring a class has one body.** A class reaches the object of the scope declaring it
as that scope's body compiled it (fact 5), so a parameter the code of a scope that declares a class
relies on, anywhere inside it, is an argument of the scope's name instead. This holds for a unit and
for a generate block alike.

**D5. A parameter handed to a child's argument is an argument of the scope building the child, of
the same layer.** One handed to an argument of what the child declares is an argument of what the
building scope declares, since that scope declares a member of the child's class; one handed to an
argument of the child's code only is an argument of the building scope's code, since which body is
built there is how the scope is built (fact 6).

**D6. The design root makes each instance from the entry of the body it is compiled to.** An
instantiation states the unit and the body it makes, the construction table it hands on is the
body's, and the entry making an object of a body is the body's, named under it. Every body's entry
of a unit is declared with the one prototype the unit published, so a table names it at that type
and erases it by a conversion it states, as it does an entry of its own.

## Limits, each with its cause

- An argument of a body only is read as the object's field where its value is known to the body (D2;
  one way of reading a supplied parameter).
- Two values a parameter takes that settle every constant alike are two units or two bodies (fact
  2).
- A scope that hands a child an argument of the child's code only is a body per value, though its
  own code is one (D5; a construction table is per body, as `Mid<P>` naming `Leaf<P>` is one class
  per `P` in C++).
- A unit or a block declaring a class is a unit per value its code relies on (D4).
- Every body compiles the published classes' constructor, destructor and definition again; the link
  keeps one (D3).
- An automatic local whose width a supplied value fixes is counted as declared, because the front
  end records the scope an evaluation was made in and not the declaration it was made for.

## Survey

Where a unit's body is compiled: C++ instantiates a template where it is used, and an explicit
instantiation (`template class X<3>;`, clang `Sema::ActOnExplicitInstantiation`) compiles it beside
the definition, in one translation unit or several; rustc makes each instance an item of the
monomorphization collector. Both name an instance by its whole arguments, and so does this. The
condition that differs is which parameters are arguments: C++ and Rust make the author write a
template argument apart from a constructor argument, and SystemVerilog writes both as `parameter`,
so the arguments are the parameters something compiled relies on, read off the front end's record.

Where what every body states is compiled: clang's header-defined class is stated by every
translation unit and kept once by the linker, and a named module's class only by its own unit (fact
7). A unit of its own for the published classes would hold three small definitions and cost a
compilation per module whether or not the unit has a second body, against the edit-compile-run loop;
what every body restates instead is what a header-defined class restates.

## Rejected

- **A name holding the constants settled from a parameter at each place, not its value.** It shared
  values that fold alike, and needed the front end to say which constant at which place every fact
  came from; a type settled from a fold the front end made only because it could is a fact it cannot
  place (fact 2).
- **A unit of its own holding what the unit published.** A compilation per module for three small
  definitions, and a class the source declares in the unit could not live there (fact 5).
- **Every body of a unit in one compilation.** Adding an instance with a new value would recompile
  every body, and bodies would not compile in parallel.
- **A class's route slots published, so a class reaches only the published part.** The published
  layout would follow a class method's body, and a method edit would rewrite every referrer's
  header.

## Revises

- [a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
  D1: a parameter something compiled relies on enters the unit's name or the body's by its value.
  D4: a parameter is read as its settled value only where it is an argument of the unit.
- [a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D5 and D7: a
  unit is realized as a block is, one level out, and a scope declaring a class has one body.
