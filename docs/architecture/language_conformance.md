# Language Conformance

What source text Lyra accepts, who decides it, and how a text nobody is sure about gets ruled on.

## The rule

**Lyra targets IEEE 1800-2023 and delegates language analysis to a chosen version of its front end.
By default it takes the front end's range of accepted text and the severity of its diagnostics, and
lowering does not judge again whether the source is legal. Lyra carries out what the front end's
analyzed representation means. Where neither the standard nor that representation's contract says
what an accepted text does, Lyra chooses the simplest behaviour. An error of analysis is corrected
in the front end, and an error of execution or a missing capability is corrected in Lyra.**

The responsibilities are split three ways.

| Layer                | Responsibility                                                                                                 |
| -------------------- | -------------------------------------------------------------------------------------------------------------- |
| Language policy      | The IEEE version, the front end version, the default options, and the compatibility modes a caller may select. |
| Front end            | Parsing, type checking, constant evaluation, language diagnostics, and the analyzed representation.            |
| Lowering and runtime | What the analyzed representation means when run, and a report for what is not yet carried out.                 |

**That the front end accepts a text means the text passed language analysis.** It does not mean Lyra
can yet carry it out, and it does not mean the front end has defined everything it does when run.

**The hand-off is the analyzed representation and its contract.** The contract is the nodes, their
types, the constants the front end evaluated, and the meaning the front end commits to for them. The
shape a tree happens to take is not part of it. Lowering keeps every decision the front end made,
even for a text the standard leaves undefined, and decides only what the front end left open.

The goal is that a text the standard allows is accepted and does what the standard says. A case that
fails it is a defect, either the front end's (reported upstream) or Lyra's (a bug, or a gap reported
as one).

Lyra adds no rule of its own accepting a text because a design runs on some other simulator. Tools
that take a text the standard forbids do not agree on which texts or on what they mean, so a rule
added for one tool copies that tool's choice, and the design then means what that tool decided.

Lyra takes the front end's defaults because language analysis then has one implementation. A second
set of rules beside it, as a table of diagnostics to raise and lower or as checks in lowering, would
drift from the first, and every difference between them would be a disagreement never argued where
it could be settled.

Consequences:

1. **The default options are the front end's, with one kind of exception.** Where the standard
   leaves a choice to the tool, the front end is not wrong whichever way it went, so nothing can be
   corrected upstream, and the policy makes the choice itself (see _Given a meaning, but a tool may
   refuse it_ below). No other default is overridden, in either direction.
2. **One class of forbidden text the front end refuses is run when a caller asks for it.** Its
   boundary is stated in advance and drawn by the standard rather than by any tool's answer. The
   standard's own rules fix what the text means, and it allows no other way to say it (see
   _Forbidden, but the defect is the standard's_ below). It stays an error by default.
3. **Lowering does not judge again whether the source is legal.** It carries out what the front end
   analyzed and never inspects the form the source wrote it in. A reader sees one of four outcomes,
   and lowering says which.

   | Situation                                                     | Reported as                                                        |
   | ------------------------------------------------------------- | ------------------------------------------------------------------ |
   | The front end refuses the source                              | The front end's diagnostic. Lowering does not run.                 |
   | The front end accepts it with no meaning (a tolerant reading) | A refusal where it was written, saying the front end gave it none. |
   | The front end accepts it and Lyra does not yet carry it out   | A gap, naming the construct.                                       |
   | The representation breaks the hand-off contract               | A compiler error, never a statement about the source.              |

4. **An accepted text with no defined meaning gets the simplest behaviour, stated where it is
   carried out.** Where the standard gives a text no meaning and the front end's contract does not
   supply one, what Lyra does with it is an execution policy, and the comment beside the code that
   carries it out is its record. It becomes a ruling below only where the choice changes what the
   program computes, and a ruling never records the front end's behaviour in full.
5. **What goes upstream is decided by evidence, not by severity.** A suspected analysis error, a
   missing diagnostic, or a meaning that contradicts the standard is reported with the clause and a
   reduction. A text the front end accepts deliberately as an extension is not a defect for being
   nonstandard. For a text the standard does not settle, Lyra's position in that discussion is the
   stricter reading.
6. **A caller may select a named compatibility mode or front end option** (`--compat`, or an option
   for a single reading, such as `--allow-hierarchical-const`). Lyra applies the corresponding
   analysis rules and stands behind the mode as it does the default. It does not promise to behave
   the same as any other simulator.

## Ruling on a disputed text

A design stops, or a text is refused that its author expected to work, and somebody has to say whose
defect it is. The steps are in this order because each one has overturned a ruling made without it.

1. **Reduce it** to a few lines that show the construct alone.
2. **Read the clause that defines the construct** and quote the sentence.
3. **Search for the clause that would make the other side right** before saying anyone is wrong. Two
   clauses can each read as the whole answer (see _One module described twice_ below).
4. **Check the reduction writes what the design writes.** A reduction that reaches a name through an
   instance, where the design reaches it through an interface port, rules on another construct.
5. **Rule**: the design is wrong, the front end is wrong, Lyra is wrong, the standard does not
   settle it, or the standard is wrong.

The ruling is argued from the standard's text and from nothing else. What an open-source tool does
with the reduction may be noted beside it, as a reason the text went unnoticed or as a second reader
of the clause, and its source can be read for the clause it relies on. It is never the reason for a
ruling: a tool that accepts says only that it accepts.

A design that is wrong is its author's to correct, and whoever builds it without being able to edit
it asks for the reading it relies on by name (consequence 4). A front end that is wrong is reported
upstream. Neither is answered by changing what Lyra accepts.

A forbidden text is not always its author's mistake. Where all three of these hold, the defect is
the standard's, the author is not reported to, and the text is run when a caller asks for it:

1. **The text is in common use.** Shown from open sources: an open tool that takes it, the front end
   listing it among the texts it relaxes for other tools.
2. **The standard already fixes its meaning.** The rules the standard gives for the texts it does
   allow, applied to this one, leave no choice for a tool to make. The test is whether running it
   needs an answer nobody wrote down: if it does, the text has no meaning to run.
3. **No text the standard allows says the same without rewriting the design.** Where an allowed form
   says it, the author is told of that form, as with any forbidden text.

Each is argued in the ruling from the standard's text and open sources, and a ruling that cannot
show all three leaves the text an ordinary forbidden one.

## The rulings

Each text below was met in a real design or in Lyra's own tests and was disputed: some tool takes
it, or some tool refuses it, against what one would expect.

### Forbidden, and refused by the front end

#### A based literal whose digits start with an underscore

```systemverilog
logic [7:0] a = 8'h_ff;
```

5.7.1: "The underscore character (`_`) shall be legal anywhere in a number except as the first
character." Write `8'hff`.

#### A comma after the last argument of a subroutine

```systemverilog
import "DPI-C" function void mem_access(
  input  bit        req_rw,
  output bit [63:0] rsp_data,
);
```

A.2.7: `tf_port_list ::= tf_port_item { , tf_port_item }`. A comma is followed by an argument.

Read to the letter, the grammar also derives an argument with nothing written in it, because every
part of an argument is optional and a prototype may omit the name. No clause gives such an argument
a meaning, and a prototype that declared one would not match the function it names. Remove the
comma.

#### A part-select whose bounds run against the declaration

```systemverilog
localparam int RomSize = 1;
logic [63:0] addr;
logic [$clog2(RomSize)-1:0] q;

q = addr[$clog2(RomSize)-1+3:3];   // addr[2:3] when RomSize is 1
```

11.5.1: "The first expression shall address a more significant bit than the second expression."

This is the text that shows why a tolerated reading cannot be borrowed. The standard says nothing of
what `addr[2:3]` selects from `[63:0]`, so a tool that takes it has to invent an answer; one open
simulator reads it as `addr[3:2]`. Write the select so that it is in order for every parameter
value.

#### A parameter set from `$bits` of a name reached through an instance

```systemverilog
module leaf;
  logic [11:0] data;
endmodule

module top;
  leaf u ();
  localparam int W = $bits(u.data);
endmodule
```

6.20.2: a value parameter "can only be set to an expression of literals, value parameters or local
parameters, genvars, enumerated names, or a constant function of these. ... Hierarchical names are
not allowed."

The neighbouring text that is allowed is `$bits` of a member reached through an interface port,
below. The same name through an instance in a declaration's width is not settled by the standard,
also below.

#### A hierarchical name through an unnamed generate block

```systemverilog
if (1) begin
  logic [3:0] held = 4'd6;
end
initial $display("%0d", genblk1.held);
```

27.6: "an unnamed generate block has no name that can be used in a hierarchical name". The name
`genblk1` exists for external interfaces to refer to the block. Name the block.

#### An expression of another type assigned to an enumerated variable with no cast

```systemverilog
typedef enum logic [3:0] { kTrue = 4'h6, kFalse = 4'h9 } mubi_t;

module leaf(input mubi_t en);
endmodule

module top;
  logic j;
  leaf u (.en(j));
endmodule
```

6.19.3: "assignment of arbitrary expressions to an enumerated variable requires an explicit cast",
and its example marks `C = I` illegal.

This is the forbidden text with the best case for an exception, since a tool that takes it has one
obvious meaning to give it. It is an error all the same, because an allowed text says the same with
nothing rewritten: `mubi_t'(j)`. A design that cannot be edited is built with
`--relax-enum-conversions`, which reads every such assignment as that cast.

### Forbidden, but the defect is the standard's

Refused by default and run when the caller asks for it by name. The author is not reported to.

#### A `force` on a member of a variable of a packed structure type

```systemverilog
typedef struct packed { logic [8:0] next; logic we; } field_t;
typedef struct packed { logic reset_b; field_t [3:0] ctrl; } hwif_t;
hwif_t hwif;

always_comb hwif = '0;

for (genvar slot = 0; slot < 4; slot++) begin : g
  initial force hwif.ctrl[slot].we = 1'b1;
end
```

10.6.2: the target "shall not be a bit-select or a part-select of a variable". 7.2.1: a packed
structure is "a mechanism for subdividing a vector into subfields", so a member of `hwif` is bits of
one vector variable. Forbidden.

All three conditions hold. The front end keeps the refusal as a warning raised to an error, in the
set it relaxes because "some tools violate the standard", and an open simulator takes the text. The
standard's rules for forcing a whole variable already say what it means: a procedural assignment
does not change what is forced while the force is in effect, and after the release the variable
keeps that value until the next procedural assignment, unless a continuous assignment or an `assign`
drives it, which is then reestablished; applied to the forced bits, nothing is left to choose. The
open simulator answers otherwise after the release of a part, taking the value `always_comb` last
wrote, while it keeps the forced value of a whole variable as 10.6.2 says, so its answer for a part
is its own. And no allowed text says the same: forcing the whole variable restates every member the
design drives, and forcing a net adds one to the design for the testbench's sake.

`-Wno-error=bad-procedural-force` asks for it, and so does `--compat all`.

### Forbidden, accepted by the front end

Run as the front end analyzed it, with whatever warning it gives, which is what tells the author.

#### A message as the first argument of `$fatal`

```systemverilog
$fatal("Randomize %s failed", name);
```

20.10: `$fatal [ ( finish_number [, list_of_arguments ] ) ]`, with `finish_number ::= 0 | 1 | 2`.
The message belongs in the list of arguments after the finish number. Forbidden.

The front end takes the first argument as the finish number, warns that it "must have value of 0, 1,
or 2" (`-Wfinish-num`), and takes the message from the second argument on. Lyra keeps both
decisions, so the string is not printed, and the simulation still ends with a failure. Write
`$fatal(1, "Randomize %s failed", name)`.

#### Case equality on a real operand

```systemverilog
real a, b;
if (a === b) ...
```

Table 11-1 gives `===` and `!==` the operands "Any except real and shortreal". Forbidden.

The front end accepts it with no diagnostic and evaluates it as `==`, and Lyra keeps that reading,
so `+0.0 === -0.0` is 1 and a NaN equals nothing. The choice changes what the program computes,
since a bit-by-bit match (12.5) would answer both the other way. Write `==`.

### Not settled by the standard, so refused

#### One module described twice

```systemverilog
module leaf;
  initial $display("the first definition");
endmodule

module leaf;
  initial $display("the last definition");
endmodule
```

Two clauses, and each reads as the whole answer. 3.13 a): "Once a name is used to define a module
... within one compilation unit, the name shall not be used again (in any compilation unit) to
declare another". 33.3.1: "If multiple cells with the same name map to the same library, then the
last cell encountered shall be written to the library ... a warning shall be issued."

33.3.1 describes compiling a cell again; 3.13 is the general rule. Nothing says which governs two
descriptions in one compilation, and one open simulator runs the first, which is neither. Remove
one.

A design that cannot be edited is built with `-Wno-error=duplicate-definition` only where every
description of the name says the same, since every reading then runs one design. Where they differ,
the front end under that option runs the last and the open simulator the first, so the text still
has no meaning to run.

#### A `force` on a member of an element of a variable

```systemverilog
typedef struct { logic we; logic [7:0] next; } field_t;
field_t ctrl [4];

for (genvar slot = 0; slot < 4; slot++) begin : g
  initial force ctrl[slot].we = 1'b1;
end
```

10.6.2: the target "can be a reference to a singular variable, a net, a constant bit-select of a
vector net, a constant part-select of a vector net, or a concatenation of these. It shall not be a
bit-select or a part-select of a variable".

The sentence lists what a target can be and names neither a member nor an array element. Whether a
member of an element is "a singular variable" is a reading, and the list is read as complete. A
member of a packed structure leaves no such reading, since it is a part-select of a variable
(above).

#### A width given by `$bits` of a name reached through an instance

```systemverilog
interface bus_if;
  logic [11:0] data;
endinterface

module top;
  bus_if u ();
  wire [$bits(u.data)-1:0] copy = u.data;
endmodule
```

20.6.2: "The `$bits` function can be used as an elaboration-time constant when used on fixed-size
data types; hence, it can be used in the declaration of other data types, variables, or nets." No
clause forbids a hierarchical name in a declaration's width.

But every clause that rules on a value fixed at elaboration forbids one: a parameter's value
(6.20.2), an enumerated name's value (6.19), the argument of `type()` (6.23). Each is the same
difficulty, a value needed to build the hierarchy taken from inside a part of it, and a width is
that difficulty as well; the standard says so of those positions and not of this one. Reach the
member through an interface port, which is not a hierarchical name (see `$bits` of a member reached
through an interface port, below), or give the width its own parameter. A design whose author reads
it the other way is built with `--allow-hierarchical-const`.

### Given a meaning, but a tool may refuse it, so run with a warning

#### A constant index or part-select outside its vector or array

```systemverilog
logic [7:0] v = 8'ha5;
logic [7:0] w = 8'h00;
int a [4];

$display("%b %b %0d", v[9], v[11:6], a[5]);
w[0 +: 70] = {70{1'b1}};
```

11.5.1 says what such a select of a vector does: one completely out of bounds "shall yield the value
x when read and shall have no effect on the data stored when written. Part-selects that are
partially out of range shall, when read, return x for the bits that are out of range and shall, when
written, only affect the bits that are in range." The same clause ends: "Bit-select or part-select
indices that are outside the declared range may be flagged as a compile-time error." 7.4.5 says the
same of an unpacked array: a read with an invalid index returns the value of Table 7-1, a write
"shall perform no operation", and "Implementations may issue a warning".

So both answers conform: refusing the select while compiling, and running it with the value the
standard gives. The front end refuses. Lyra runs it and warns, because the text is common in designs
that are correct: code written for a larger configuration selects past the end of a vector when it
is built with a smaller one, at a position that is constant only once the parameters are known. A
part-select wider than its vector, such as `w[i +: 70]`, is the same case, since part of it lies
outside wherever `i` is. A caller who wants the refusal asks for it by name, `-Werror=index-oob`,
`-Werror=range-oob` and `-Werror=range-width-oob`.

### Allowed

#### An `always_comb` writing one constant key of an associative array

```systemverilog
int by_name [string];

always_comb by_name["out"] = by_name["in"] + 1;
initial by_name["in"] = 4;
```

9.2.2.2: variables an `always_comb` assigns "shall not be assigned by any other process. ...
However, multiple assignments made to independent elements of a variable are allowed as long as
their longest static prefixes do not overlap". 11.5.3: "An indexing select is a static prefix if the
indexing select prefix is a static prefix and the select expression is a constant expression."

`by_name["out"]` and `by_name["in"]` are two static prefixes that do not overlap. The clause makes
no exception for an associative array.

#### `$bits` of a member reached through an interface port

```systemverilog
interface bus_if;
  logic [11:0] data;
endinterface

module reader(bus_if b);
  wire [$bits(b.data)-1:0] copy = b.data;
endmodule
```

11.2.1: calls to the data query functions "may also be constant system function calls even when
their arguments are not constant". 20.6.2: "The `$bits` function can be used as an elaboration-time
constant when used on fixed-size data types; hence, it can be used in the declaration of other data
types, variables, or nets."

No clause forbids it. 6.20.2's ban on hierarchical names is about what a parameter is set from, and
the interface a port carries is fixed where the module is instantiated (25.3.3), so the member's
type is known at elaboration.

## Forbidden shapes

- **A default in Lyra's policy that overrides the front end's acceptance or severity**, except where
  the standard leaves the choice to the tool. It is a second statement of the standard that nobody
  argues with the front end.
- **A refusal in lowering phrased as a rule about the source.** Lowering reports one of the outcomes
  consequence 3 lists, and says which.
- **A ruling whose stated reason is that a tool accepts or refuses the text.** A tool is at most a
  second reader of the clause, and tools that take a forbidden text do not agree on what it means.
- **A carried change to somebody else's design that only suits Lyra.** A design that is wrong is
  wrong for every tool that follows the standard, and the correction is one its maintainer can take.
