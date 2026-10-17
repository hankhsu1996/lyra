# A parameter read only as a value is supplied at construction

Date: 2026-09-24 Status: accepted; D1 and D2 revised 2026-10-11 and again 2026-10-12, D3 and D7
withdrawn 2026-10-12, D1 and D4 revised 2026-10-13: a parameter something compiled relies on enters
the key by its value; D1 and D2 revised 2026-10-14 by
[a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md):
a unit holds the generate blocks its own instances built, which are part of its name, and no
instance is lowered into another's unit

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
down, for a block's own constant computed from its index. A module boundary added the one thing that
record did not face: a parent named the child it built without reading the child's body, so whatever
decided sharing had to be known before the body was.

That premise does not hold where a parent is handed a construction table saying which body is made
at each of its instantiations: its code names no body, so two parents that differ only in which body
of a child they make are one body, and what a value decides is a question about the child alone --
except where it decides which generate blocks the child builds, which names the child's unit and so
the parent's class
([a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)
D4).

## The tension this addresses

`specialization_model.md` already says what a value that decides nothing is: a constructor input,
not part of the key. The standard agrees -- a parameter is fixed before the run (LRM 6.20, 23.10),
and nothing requires the compiled class to hold it. What stood in the way was two things.

**The name comes before the comparison.** Inside a unit, blocks were then compared after they were
lowered and nobody had to name them first. Across units the parent writes the child's name while it
lowers, and it may read the child's declarations but not its body. So the split between what enters
the key and what is handed at construction is a fact the parent computes from the child's
instantiation, by the same function the child uses for itself -- a prediction, where the loop record
then compared.

**A prediction is a list, and a list of how SystemVerilog lets a value reach a class has no end.**
What made the loop record sound was declining to predict. Here the prediction cannot be declined, so
it is not written as a list at all: it is read off the front end's own record of what each constant
evaluation read (D1).

## Decision

### D1. A parameter is read as the source wrote it, except where something compiled relies on it

A reference to a parameter is an expression the source wrote, and this compiler lowers it as that
expression: the object reads the value it was built with, as a C++ object reads a constructor
argument. Where something compiled relies on the parameter's value -- a constant the language
requires, such as a declared range, a replication count or a range select's width, or a type the
front end reads as a fact, such as what `type(...)` declares or what `$left(...)` answers from --
the parameter is an argument of the specialization instead, as a C++ template argument is, and its
value enters the key: the unit's name where what the unit declares relies on it, the name of the
body the unit is compiled to where only its code does
([a-unit-is-compiled-to-a-body-per-value-its-code-relies-on](a-unit-is-compiled-to-a-body-per-value-its-code-relies-on.md)).
SystemVerilog writes both kinds as `parameter`, where C++ and Rust make the author say which, so
which one a parameter is is read off what relies on it. A reference anywhere else adds nothing to
the key, and that includes what a construction evaluates to decide what it builds: a conditional
generate's condition, a `case` generate's selector and labels, and a loop generate's header (LRM
27.4, 27.5). What an instance built is part of its unit's name instead
([a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)
D1), so two values that build the same blocks are one unit. A parameter stays in the key as well
where an instantiation may override it and it is written with no type (LRM 6.20.2: its type is the
type of the value it is given), where it is handed on to a child parameter that decides something
there, where a design element declared inside this one reads it, where it is read through a
hierarchical name, and where another deciding parameter is written from it -- including one the
module's blocks or subroutines declare.

**What relies on a parameter is the front end's own record, not a list kept here.** A list of a
language's positions kept beside the code that decides them -- the positions this compiler lowers as
an expression, or those the front end settles a constant at -- goes stale: each time a construction
learns to run something -- its conditions
([a-conditional-generate-chooses-at-construction](a-conditional-generate-chooses-at-construction.md)),
its loops ([one-body-built-at-every-index](one-body-built-at-every-index.md) D2) -- or the front end
folds somewhere new, a parameter is classified wrongly and nothing shows it. So the front end
records, as it evaluates a constant the language requires, which parameters that evaluation read,
and marks the reads that only decide which blocks a construction builds. Where it reads a type as a
fact -- the type `type(...)` declares, an argument a system subroutine reads for its type alone, the
part a port names -- it records the parameters its folds read while settling that type, since a type
can be settled from a fold made only because it could be: a part-select's position, a condition
leaving an arm unevaluated. The classification reads that record: a parameter some evaluation read
is an argument, unless the read only decides blocks, or sits where the lowering states the
expression itself -- an element select's index and an indexed part select's base, which name a
position rather than a shape (LRM 11.5.1, 11.5.3), and an input port's default, which each object
evaluates (LRM 23.2.2.4). Nothing here names the positions the language requires a constant at, so
none can be missed. A select's position the front end found out of bounds is the exception: such a
select reaches nothing, which is what is compiled, so the front end records the position as a
required constant and it decides. Two blocks of one loop that differ only there are two bodies; the
select is rare and warned of, so no more than that is done for it.

The key holds an argument's value, not the constants settled from it at each place. Keying by the
constants would share two values that size a range alike, but it needs the front end to say, for
every fact, which constant at which place it came from, and the places a type is settled from a fold
it made only because it could are exactly the ones it cannot say. Keyed by the value, a parameter is
an argument or it is not, which the record of what was read answers whole. Two values that size a
range alike are two units, which is what a C++ template does and what rustc's `Instance` does with
its arguments.

A value given elsewhere -- by a `defparam` (LRM 23.10.1) or a configuration (LRM 33.4.3) -- is
classified the same way as the instantiation's own, since LRM 23.10 makes the three ways of altering
a parameter one thing. What differs is only who writes the value the parent hands: the
instantiation's expression is lowered where it is written, while a value given elsewhere was written
in another scope, so the parent hands the constant it settled to, which its own key holds
([specialization-identity](specialization-identity.md) decision 1). So such a parameter is not kept
in the key: what the parent hands differs from what the parameter holds as an expression, never as
the constant. A top-level instance is built by the design root, which hands nothing, and is the only
instance of its unit, so every value it is given is compiled in.

A class specialization or a virtual interface is chosen by the values a site hands it and shared by
every site handing the same ones, so a parameter whose value is read into one is an argument; the
record answers that too, since the front end charges the values a site hands a specialization to the
instance writing the site.

The record is the front end's answer to a question about its own evaluation, which is what
[an-elaboration-time-value-is-an-input](an-elaboration-time-value-is-an-input.md) D1 asks of every
place this compiler takes an answer the front end settled. A place where this compiler itself reads
the front end's constant for an expression the source wrote is a fold of ours, and is undone where
it is found.

### D2. Every instance of a unit lowers alike, and the check of the compile against itself holds them to it

**What an instance's construction built is part of its unit's name**
([a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)
D1), so every instance of a unit built the same blocks, and a unit is lowered and declared from the
instance it is named for alone.

They agree by construction, because the lowering reads a value only through what the unit's identity
holds or as one the object is built with; that is what D1 makes the classification answer. Where two
of them still lower to anything else, the classification missed something the front end's record did
not show, which is this compiler's defect: it is reported as one, naming the instance, and nothing
falls back to a unit per instance. Keeping a definition whole wherever its instances differ would
make the comparison the answer and every classification a guess it corrects; with the record of D1
there is no guess to correct, and a fall back would hide the one defect that remains.

A class the design element holds is no exception. Each instance's copy is a type of its own (LRM
8.25), but every copy at one place in the unit is the same text, so they are one class of the unit,
declared from the instance the unit is named for. Within that instance, a class each of a loop's
blocks declares is one class of the unit, built from one block holding a copy; where the compile
checks itself every copy builds it, and each has to come out the same. That is how clang merges a
class two modules each define: the later definition is looked up in its context by name, found to be
the same entity, and folded into the first (`ASTDeclReader::findExisting` and
`ASTDeclMerger::MergeDefinitionData` in `clang/lib/Serialization/ASTReaderDecl.cpp`).

An instance of a unit an earlier instance is already of is not read: it builds what that instance
does. Reading every instance through its own body and lowering each into its unit is how a compile
is checked against itself, and the tests do that.

The agreement is only as good as the equality, so HIR equality is derived and exact: every node's
`operator==` is the defaulted one, and a real value is held as the bits that represent it. A number
compares 0.0 and -0.0 equal, and dividing by each gives infinities of opposite sign, so a comparison
by value would call two different programs one. `tools/policy/check_architecture.py` A025 holds
this.

### D3. Withdrawn: a definition kept whole was said as a remark

With nothing kept whole (D2) there is nothing to remark on, and the diagnostic kind and the option
that asked for it are gone.

### D4. Only what varies with a supplied value is declared

A supplied parameter is a declaration of its unit, filled from the value the construction hands it;
so is a parameter whose declaration writes it from one, holding that expression. One a subroutine or
a procedural block declares is a static-lifetime declaration of that body, as a static variable
there is (LRM 6.21): one per object, initialized with its expression when the object is built, and
read by name. That is what a C++ compiler makes of a `static const` local whose initializer is not a
constant expression -- a variable of its function, initialized once, never a value folded into each
read. An argument of the unit's signature -- including one a design element declared inside this one
reads, one whose type is its value's, one a class reads, and every parameter of a top instance -- is
read as the value its specialization settled, because that value is in the key and reading it costs
no second unit. An argument only of a body is supplied like any other and read as what the object
was built with, which is the value the body's name holds. A design with nothing supplied lowers
exactly as before.

### D5. A construction hands its values to the entry by type

The unit's object entry takes the supplied values after the parent, the segment and the construction
table, in the order the child declares them, and passes them to the constructor. Both sides reach
that order and those types from the child's instantiation, and the child publishes them, as a class
publishes its constructor, for whoever names the entry without building through it: the design root,
whose construction tables hold every unit's entry. The design's tops are handed no values.

Every construction states its constructor arguments where the construction is written: for an
instance, one per parameter it takes -- the value its instantiation assigned or that was given it
elsewhere, the latter as a constant, or that none was, which leaves the parameter to its default;
for a loop's generate block, its index (LRM 27.4); for any other block, nothing, since a block has
no parameter ports (Syntax 27-1). One argument per value the built scope receives, in the order it
receives them, so the lowering below translates what was stated and never works out which value goes
where. This is how a C++ front end holds a construction -- clang's `CXXConstructExpr` carries its
arguments at the site and names the constructor whose declaration holds the parameters -- and it is
the one shape for both kinds of scope, so the count a scope receives and the count its builder hands
are each stated once, on their own side.

Whether an instantiation assigned a parameter does not tell instances apart, so a parameter left to
its default is supplied like one assigned. The parameter is passed as whether the instantiation
assigned it, the source's own `.P( [ param_expression ] )` (LRM 23.10.2.2), beside the value, which
is the type's default where none was -- what C++'s `std::optional` holds, a flag and a slot for the
value. It crosses as a tuple and not as a sum, since a union of MIR's is a declaration the source
states and this is not, so one argument states the protocol on both sides. Where none was assigned,
the unit fills the parameter from the default its declaration states (LRM 6.20.1), evaluated in its
own scope against the values its earlier parameters hold. That is where clang keeps a default
argument too, on the callee's parameter rather than at the call (`ParmVarDecl`'s default argument,
instantiated only where a call uses it). The front end binds a default only where the parameter was
not overridden, since an unused default need not hold against the values the other parameters were
given; it is given the default of an overridden parameter, bound in the parameter's own scope with
nothing it reports kept, by a change carried in this project's fork. A default that is not valid in
a unit is one no instance of it can be left to, since what makes it invalid is part of which unit it
is.

[one-body-built-at-every-index](one-body-built-at-every-index.md) D4 made a scope's entry take its
values erased and counted, because the constructing site held only the definition and not the
prototype. A parent computes which values it hands and of which types from the same function the
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

### D7. Withdrawn: a unit per value that decides nothing was said as a remark

It lowered each definition compiled more than once again under a classification handing it every
value, and remarked where the units came out one body. With D1 reading the front end's record, a
parameter is kept in the key only where an evaluation read it, so what this trial looked for is a
defect of the record, found where any defect is: the designs the tests hold to their size grow.

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
- **Keep a definition whole where its instances lower apart.** Sound while the classification is a
  list kept here, when a miss is expected. Read off the front end's record, a miss is a defect in
  this compiler or in that record, and a fall back that still builds the program is what would keep
  it from ever being found.

## Consequences

- The reduction above emits one `Leaf` and a parent of 21 KB at 256 iterations: 33.7 KB of design
  C++ against about 4.4 MB, measured at `16709334` plus this change. Both backends print each
  instance's own value.
- A parameter read only to decide what an instance builds is supplied. Measured on one module given
  three values, a unit each before and one after: read in a module-level loop's bound, in an `if`,
  in the same `if` inside a loop, in a `case`, and through a `localparam` the module writes from it.
  Instances selecting different alternatives are a unit per set of blocks they built. A loop of
  instances each handed its index, where the index selects an alternative, a case item and a loop
  count in the child, is one of the designs held to its size: the child is a unit per set of blocks
  its values build, and the block holding it a class per index, since the index names which unit the
  child is.
- A parameter in a declared range is an argument, so each value is a unit of its own, even where two
  values size the range alike.
- Whether a parameter is an argument is found by the front end's record of what relied on it rather
  than by a walk of where it could be. The front end is ours, so that is where the record was added.
- A module holding a class is a unit per set of blocks its instances build, as any module is, and a
  class a loop's block declares is one class of the unit however many blocks hold a copy, wherever
  the index decides nothing about the block.
- [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) is
  superseded for value parameters: the constructor-input vehicle it waited for is the one the loop
  record built.
