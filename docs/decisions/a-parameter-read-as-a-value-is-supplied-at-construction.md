# A parameter read only as a value is supplied at construction

Date: 2026-09-24 Status: accepted

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

### D1. The classification asks where the value reaches, and denies by default

A value parameter is supplied at construction when every reference to it sits where this compiler
lowers the expression the source wrote and the run evaluates it -- an operand of a statement, a
continuous assignment, a variable's initializer, an input port's default, or the initializer of a
parameter the unit declares -- in its body, a generate block, a subroutine or a procedural block --
which is computed when the object is built -- and never where its value fixes a type: a declared
range, a replication count, a range select's width. A reference anywhere else keeps it in the key: a
generate condition or loop header, a port connection, an instance array's range, a place nobody
listed. So does a parameter an instantiation may override written with no type (LRM 6.20.2: its type
is the type of the value it is given), one handed on to a child parameter that decides something
there, and one another deciding parameter is written from -- including one the module's blocks or
subroutines declare.

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
positions SystemVerilog lets a value reach are an open set, which is why it denies by default, and a
constant the front end settles at a site nobody has found yet leaves nothing for the walk to read.
What makes it safe is D2. What it is for is making D2's fallback rare, and each site found is closed
the same way: the front end keeps the expression beside the constant, as
[an-elaboration-time-value-is-an-input](an-elaboration-time-value-is-an-input.md) D1 asks of every
place this compiler takes an answer the front end settled.

### D2. The comparison decides correctness, not the classification

Every instance handed values no earlier instance of its unit was handed is lowered as a unit of its
own would be, and compared with the unit it shares, through the equality the node definitions
derive. Where one differs, the definition is kept whole, with every parameter in the key -- where it
was before this record -- and the design is declared again with that answer. So an incomplete
classification costs sharing, and the program is the one each instance's own body describes.

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
- A parameter a conditional generate reads stays in the key: the alternatives an instance did not
  select have no body in its elaboration, so instances selecting differently lower apart.
- A parameter reaching a constant the front end settled -- the positions D1 lists -- stays in the
  key, and the classification rather than the comparison finds it, since the expression each
  constant was settled from is kept beside it. The front end is ours, so that is where each was
  added.
- [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) is
  superseded for value parameters: the constructor-input vehicle it waited for is the one the loop
  record built.
