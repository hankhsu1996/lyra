# A parameter read only as a value is supplied at construction

Date: 2026-09-24 Status: accepted; D1 and D2 revised 2026-10-11, D7 added

## Context

A module instantiated with a parameter whose value differs per instance compiled to one unit per
value, however alike the values lowered. The shape real RTL writes it in is a loop generate handing
each child its own index:

```systemverilog
module Leaf #(parameter int K = 0) (input bit c, output int o);
  always @(posedge c) o <= K;
endmodule

module Top;
  bit clk;
  int o [256];
  for (genvar i = 0; i < 256; i++) begin : g
    Leaf #(.K(i)) u (.c(clk), .o(o[i]));
  end
endmodule
```

Measured as a C++ emit: 256 units of `Leaf` of about 10.6 KB each, and a parent of 1.68 MB -- the
loop around them could not be one body, because each block named a different child. About 4.4 MB of
design C++ where one `Leaf` serves every index. A large design measured outside the project had most
of its emitted text in modules like this one.

[one-body-built-at-every-index](one-body-built-at-every-index.md) removed the same growth one level
down, for a block's own constant computed from its index. A module boundary adds the one thing that
record did not face: a parent names the child it builds without reading the child's body, so
whatever decides sharing has to be known before the body is.

## The tension this addresses

`specialization_model.md` already says what a value that decides nothing is: a constructor input,
not part of the key. The standard agrees -- a parameter is fixed before the run (LRM 6.20, 23.10),
and nothing requires the compiled class to hold it. What stood in the way was two things.

**The name comes before the comparison.** Inside a unit, blocks are compared after they are lowered
and nobody had to name them first. Across units the parent writes the child's name while it lowers,
and it may read the child's declarations but not its body. So the split between what enters the key
and what is handed at construction is a fact the parent computes from the child's instantiation, by
the same function the child uses for itself -- a prediction, where the loop record could compare.

**A prediction is a list, and a list of how SystemVerilog lets a value reach a class has no end.**
What made the loop record sound was declining to predict. Here the prediction cannot be declined, so
it is kept from deciding correctness.

## Decision

### D1. A parameter is read as the source wrote it, except where the front end settled a constant

A reference to a parameter is an expression the source wrote, and this compiler lowers it as that
expression: the object reads the value it was built with. So a value parameter is supplied at
construction unless one of its references sits where what is compiled holds a constant the front end
settled from it instead of the expression -- a declared range, a replication count, a range select's
width, and the positions listed below. A reference anywhere else keeps the parameter out of the key,
and that includes what a construction evaluates to decide what it builds: a conditional generate's
condition, a `case` generate's selector and labels, and a loop generate's header (LRM 27.4, 27.5). A
unit holds every alternative and every loop body any of its instances built (D2), so which one an
instance selects is that instance's own. A parameter stays in the key as well where an instantiation
may override it and it is written with no type (LRM 6.20.2: its type is the type of the value it is
given), where it is handed on to a child parameter that decides something there, where a design
element declared inside this one reads it, where it is read through a hierarchical name, and where
another deciding parameter is written from it -- including one the module's blocks or subroutines
declare.

**What is listed is where the front end folds, and that is a fact about the front end.** This record
first listed the opposite: the positions this compiler lowers as an expression, with every other
reference keeping the parameter in the key. That list restated what the lowering does, in a walk
made before the lowering ran, and nothing tied the two together. It went stale twice --
[a-conditional-generate-chooses-at-construction](a-conditional-generate-chooses-at-construction.md)
made a construction run its conditions, and
[one-body-built-at-every-index](one-body-built-at-every-index.md) D2 made it run its loops -- and
each time a parameter read only there went on naming a unit per value, which no test could see
because a unit per value runs the same program. A list of where the front end folds moves only when
the front end does, and each fold this compiler undoes takes a line out of it.

A value given elsewhere -- by a `defparam` (LRM 23.10.1) or a configuration (LRM 33.4.3) -- is
classified the same way as the instantiation's own, since LRM 23.10 makes the three ways of altering
a parameter one thing. What differs is only who writes the value the parent hands: the
instantiation's expression is lowered where it is written, while a value given elsewhere was written
in another scope, so the parent hands the constant it settled to, which its own key holds
([specialization-identity](specialization-identity.md) decision 1). This record used to keep such a
parameter in the key, on the ground that the value the parent would hand is not the one the
parameter holds; that is true of the expression and not of the constant. A top-level instance is
built by the design root, which hands nothing, and is the only instance of its unit, so every value
it is given is compiled in.

Some positions take a constant the front end settles while binding, so the value sits there with no
reference left in the expression: a declared range -- wherever the type is written, including a
named type, a struct or union member, an enum's base, a type written as an operand, and an interface
port's array -- an instance array's range, an associative array's index type, a size cast's width, a
hierarchical name's index or range, a streaming concatenation's slice size, a sequence's delay or
repetition, a queue's bound, the operand a type is taken from (`type(expr)`, LRM 6.23), the type a
cast or an assignment pattern names and a pattern's type key, the path a defparam names its target
through. The front end keeps beside each constant the expression it was settled from, and the
classification reads that expression as it reads any other reference -- so a parameter found there
keeps its value in the key.

A class specialization or a virtual interface is chosen by the values a site hands it and shared by
every site handing the same ones, so the specialization holds whichever site created it. What a site
wrote -- in a declared type, a name scoped through the class, a class-scoped `new`, an `extends` or
`implements` clause, a virtual interface type -- is kept on what that site produced, and read there
the same way.

**This is a question asked of the source, so it is not a proof, and it is not meant to be one.** The
positions the front end settles a constant at are an open set, and one nobody has found yet leaves
nothing for the walk to read, so a parameter folded there is taken for a supplied one. What makes
that safe is D2, which finds it and says so. Each site found is closed the same way: the front end
keeps the expression beside the constant, as
[an-elaboration-time-value-is-an-input](an-elaboration-time-value-is-an-input.md) D1 asks of every
place this compiler takes an answer the front end settled. A place where this compiler itself reads
the front end's constant for an expression the source wrote is found the same way, and is a fold of
ours to undo.

### D2. The comparison decides correctness, not the classification

Every instance handed values no earlier instance of its unit was handed is lowered with the unit and
held against it, through the equality the node definitions derive. **An instance is one body with
its unit where the two differ in nothing but what their constructions decide**: which alternative of
a conditional generate stood, and how many blocks a loop counted out. That is the relation
[a-conditional-generate-chooses-at-construction](a-conditional-generate-chooses-at-construction.md)
D2 gives two blocks of one loop, asked of two instances of one unit, and the unit is what it gives a
body: everything the instances agree on, with every alternative and every loop body any of them
built. So a unit is lowered from the instance it is named for and from each other instance that
built something it did not yet hold, into one unit, and what the unit publishes is declared from all
of them, so a name reaching into an alternative finds it whichever instance selected it. Where an
instance is not one body with its unit, the definition is kept whole, with every parameter in the
key -- where it was before this record -- and the design is declared again with that answer. So an
incomplete classification costs sharing, and the program is the one each instance's own body
describes.

Two units' scopes can be held against each other only when both name one unit's types, so the
instances are lowered into one unit for it. A class a design element holds is declared once per
unit, from one body, so a unit holding one -- declared in the module, or a specialization of a
generic placed in it -- holds what one instance built and no other. Its instances are held to it the
earlier way, each lowered as a unit of its own and compared whole, which admits instances that build
alike. Where one differs, the definition is first declared again with what a construction reads to
decide what it builds keeping the parameter in the key, as it did for every definition before, and
nothing is remarked on: that is the limit just stated and no defect of the classification. Only a
definition whose instances still differ after that is kept whole.

An instance handed the same values as an earlier one is compared too, where it is read through a
body of its own. One the front end left sharing an earlier instance's body is not read: it is one
application with that instance, and what its body would state is what the shared one does. Skipping
the comparison for an instance with a body of its own rested on a second prediction -- that a
definition, its key and its supplied values are everything a lowering can depend on -- and that one
was unchecked: a wait through an interface port was routed from where the name landed in the first
instance, so two instances agreeing on all three lowered differently and the second was built as the
first. Such an instance has no fallback, because nothing a unit is told apart by separates it from
the one it repeats, so no name exists to give each its own unit; a difference is reported as this
compiler's defect, naming the instance and the first line the two forms disagree on. What it costs
is a lowering per instance read and nothing after it, which is the axis North Star invariant 2
leaves out: 0.09 s on Ibex's 1.06 s front half.

The comparison is only as good as the equality, so HIR equality is derived and exact: every node's
`operator==` is the defaulted one, and a real value is held as the bits that represent it. A number
compares 0.0 and -0.0 equal, and dividing by each gives infinities of opposite sign, so a comparison
by value would call two different programs one. `tools/policy/check_architecture.py` A025 holds
this.

The check runs once every unit has declared and before any unit is handed on, because what it
decides -- which definitions are kept whole -- changes the names a parent's construction writes. It
holds one unit and one instance beside it at a time, so a unit's HIR still exists only while it is
in flight, as [the-front-end-has-one-reader](the-front-end-has-one-reader.md) D1 bounds it. Lowering
the instances is the work done before this record, when each was its own unit. What the check adds
is one more lowering of each unit that has such instances, in its own turn after the check; keeping
the checked unit for that turn instead would hold every such unit across the barrier, which is the
peak that record removed.

### D3. A definition kept whole is a defect in Lyra, said as a remark

The program is right and only the sharing is lost, which is Lyra's to fix, so it is a remark against
the definition: a diagnostic of its own kind, beside error, warning and note, that reaches the
terminal only when a run asks for remarks with `--remarks`. The conformance run always asks and
fails a case that remarks on lost sharing, and a measurement of a real design asks in order to count
them. This is how clang reports a transformation a pass failed to make (`-Rpass-missed`, a remark
class distinct from warning in its diagnostic mappings) and what GCC's `-fopt-info-missed` prints;
the switch is general because a missed opportunity is a category, and lost sharing is its first
member.

### D4. Only what varies with a supplied value is declared

A supplied parameter is a declaration of its unit, filled from the value the construction hands it;
so is a parameter whose declaration writes it from one, holding that expression. One a subroutine or
a procedural block declares is a static-lifetime declaration of that body, as a static variable
there is (LRM 6.21): one per object, initialized with its expression when the object is built, and
read by name. That is what a C++ compiler makes of a `static const` local whose initializer is not a
constant expression -- a variable of its function, initialized once, never a value folded into each
read. Every other parameter is read as the value its specialization settled, because that value is
in the key and reading it costs no second unit -- and a design with nothing supplied lowers exactly
as before.

### D5. A construction hands its values to the entry by type

The unit's object entry takes the supplied values after the parent and the segment, in the order the
child declares them, and passes them to the constructor. Both sides reach that order and those types
from the child's instantiation. The design's tops are handed nothing and keep the prototype the
runtime calls.

Every construction states its constructor arguments where the construction is written: for an
instance, the values its instantiation overrides or that were given it elsewhere, the latter as
constants; for a loop's generate block, its index (LRM 27.4); for any other block, nothing, since a
block has no parameter ports (Syntax 27-1). One expression per value the built scope receives, in
the order it receives them, so the lowering below translates what was stated and never works out
which value goes where. This is how a C++ front end holds a construction -- clang's
`CXXConstructExpr` carries its arguments at the site and names the constructor whose declaration
holds the parameters -- and it is the one shape for both kinds of scope, so the count a scope
receives and the count its builder hands are each stated once, on their own side.

[one-body-built-at-every-index](one-body-built-at-every-index.md) D4 made a scope's entry take its
values erased and counted, because the constructing site held only the definition and not the
prototype. A parent now computes which values it hands and of which types from the same function the
child does, so for a unit the prototype is available and the values cross typed. A generate block's
construction takes its one supplied value through the same list.

### D6. A port's default is asked of the module that declares it

LRM 23.2.2.4 evaluates an input port's default "in the scope of the module where [it is] defined,
not in the scope of the instantiating module", so the default is the child's expression and may read
the child's own supplied parameters. The child evaluates it in a subroutine of its own and publishes
that subroutine on the port; an instance leaving the port unconnected is driven once from a call of
it on the child object. The parent states no value, so its text is the same whatever the child is
handed. A C++ compiler keeps a default argument on the callee's parameter and has the call site name
it rather than restate it; the difference here is that the parent cannot name the child's
declarations at all, only what the child published, so the default crosses as the call. A name a
view offers only for reading is the same case (LRM 25.5.4) and crosses the same way.

A subroutine a unit makes up for this is named with a space, which no SystemVerilog identifier can
hold, escaped ones included (LRM 5.6.1), so it cannot collide with anything the unit declares.

### D7. A unit per value that decides nothing is said as a remark too

D2 finds a parameter taken for a supplied one that something compiled holds. Nothing found the
opposite, a parameter kept in the key that decides nothing: it costs a unit per value and changes no
program, so no run shows it and only a measurement of a real design ever did. Where a run asks for
remarks, the units of each definition compiled more than once are lowered into one unit under a
classification that hands that definition every value it is given, and held against each other as D2
holds instances. Where they are one body, a remark names the definition, of the same kind as D3's
and failing a conformance case the same way.

It is a check and never the answer. Naming a unit by how the design's other instances of it lowered
is the alternative rejected below, for the same reason: a parent's name for its child would depend
on instances the parent does not hold. **That reason holds while a unit's name is also what decides
which instances share an artifact.** `../architecture/incremental_build.md` makes those two values,
a key and a fingerprint, and under it this comparison is the answer: a parent names a child by what
its instantiation states, and keys that lower alike share what follows, with nothing classified
beforehand. This record's prediction, and the fall backs around it, are what one value doing both
jobs costs. And it is all or nothing for a definition, since it hands over every value at once: a
definition with one parameter that sizes a type and another kept for nothing comes out apart and is
not named. Left out, because the trial cannot be run on them, are a definition already kept whole,
one holding a class, and one holding an instance whose unit is named through its own.

## How others do it

- **Verilator** clones a module once per distinct parameter tuple before optimizing, and constant
  propagation into each clone is part of how it runs fast. Its objective is simulation speed; ours
  is the edit-compile-run loop, where a clone per value is the cost.
- **A C++ compiler and linker** instantiate a template per non-type argument and fold identical
  machine code at link time (lld `--icf`). The folding happens after the host compile, which is the
  cost being removed here.
- **Rust** carried a per-definition query of which generic parameters a body uses, so that instances
  differing only in unused ones could share, and removed it (rust-lang/rust#133883) for delivering
  almost nothing: a Rust generic parameter that is used nearly always decides layout. An SV value
  parameter nearly always does not, which is the opposite trade.
- **Swift** compiles generic code once and passes type metadata at run time, leaving specialization
  to an optimizer pass -- the shared form as the baseline, which is this record's direction.

## Rejected alternatives

- **Name a child by a hash of its lowered body.** Exact, and nothing predicts it; but every edit to
  a child's body changes its parent's text, so a parent compiles again whenever any child changes.
- **Group instances by comparing their bodies across the design.** A parent's name for its child
  would then depend on how other parents instantiated the same child -- a dependency no unit states.
- **Make every parameter a declaration.** Uniform, but every read of a parameter whose value the key
  already holds becomes a field read the run pays for, in exactly the loops a simulation spends its
  time in.
- **Report a classification miss as a compiler error.** The program is correct; rejecting it for a
  sharing miss is making an optimization a correctness precondition.

## Consequences

- The reduction above emits one `Leaf` and a parent of 21 KB at 256 iterations: 33.7 KB of design
  C++ against about 4.4 MB, measured at `16709334` plus this change. Both backends print each
  instance's own value.
- A parameter read only to decide what an instance builds is supplied. Measured on one module given
  three values, a unit each before and one after: read in a module-level loop's bound, in an `if`,
  in the same `if` inside a loop, in a `case`, and through a `localparam` the module writes from it.
  Instances selecting different alternatives are one unit holding both. A loop of instances each
  handed its index, where the index selects an alternative, a case item and a loop count in the
  child, is one of the designs held to its size: at 512 instances and at 1024 its units, files and
  bytes do not follow the count.
- A parameter in a declared range is told apart by its value, so two values that size the same range
  are two units that are one body. D7 says so; telling such a unit apart by what the value sizes, as
  a loop's index already is, is not done.
- A parameter reaching a constant the front end settled -- the positions D1 lists -- stays in the
  key, and the classification rather than the comparison finds it, since the expression each
  constant was settled from is kept beside it. The front end is ours, so that is where each was
  added.
- [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) is
  superseded for value parameters: the constructor-input vehicle it waited for is the one the loop
  record built.
