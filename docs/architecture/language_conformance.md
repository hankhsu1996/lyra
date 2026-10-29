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
2. **One class of forbidden text the front end refuses is run when a caller asks for it, as a
   compatibility extension.** Its boundary is stated in advance and drawn by the standard rather
   than by any tool's answer: the text is in common use, the meaning Lyra gives it is stated here
   and built from the standard's rules for the nearest text it allows, and no allowed text says the
   same (see _Forbidden, run as an extension when asked_ below). It stays an error by default.
3. **Lowering does not judge again whether the source is legal.** It carries out what the front end
   analyzed and never inspects the form the source wrote it in. A reader sees one of four outcomes,
   and lowering says which.

   | Situation                                                                                         | Reported as                                                                    |
   | ------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
   | The front end refuses the source                                                                  | The front end's diagnostic. Lowering does not run.                             |
   | The front end accepts it and builds nothing to carry out (a tolerant reading, or nothing relaxed) | A refusal where it was written, saying the text has no behaviour to carry out. |
   | The front end accepts it and Lyra does not yet carry it out                                       | A gap, naming the construct.                                                   |
   | The representation breaks the hand-off contract                                                   | A compiler error, never a statement about the source.                          |

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
   settle it, or the text is forbidden and run as an extension when asked.

The ruling is argued from the standard's text and from nothing else. What an open-source tool does
with the reduction may be noted beside it, as a reason the text went unnoticed or as a second reader
of the clause, and its source can be read for the clause it relies on. It is never the reason for a
ruling: a tool that accepts says only that it accepts.

A design that is wrong is its author's to correct, and whoever builds it without being able to edit
it asks for the reading it relies on by name (consequence 6). A front end that is wrong is reported
upstream. Neither is answered by changing what Lyra accepts.

A forbidden text is not always worth refusing outright. Where all three of these hold, the author is
not reported to, and the text is run when a caller asks for it, as an extension of the standard
whose meaning is Lyra's:

1. **The text is in common use.** Shown from open sources: an open tool that takes it, the front end
   listing it among the texts it relaxes for other tools.
2. **The meaning Lyra gives it is built from the standard's rules for the nearest text it allows,
   and stated in the ruling.** The standard gives a forbidden text no meaning, so tools that take it
   may answer differently, and the ruling says which answer Lyra gives and from which rules. Where
   no allowed text is near enough to lend its rules without an answer nobody wrote down, the text
   has no meaning to run.
3. **No text the standard allows says the same without rewriting the design.** Where an allowed form
   says it, the author is told of that form, as with any forbidden text.

Each is argued in the ruling from the standard's text and open sources, and a ruling that cannot
show all three leaves the text an ordinary forbidden one.

## The rulings

Each text below was met in a real design or in Lyra's own tests and was disputed: some tool takes
it, or some tool refuses it, against what one would expect, or two readings of the clause give it
different meanings.

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

### Forbidden, run as an extension when asked

Refused by default and run when the caller asks for it by name, with the meaning stated here. The
author is not reported to.

#### A `force` on part of a variable

```systemverilog
typedef struct packed { logic [8:0] next; logic we; } field_t;
typedef struct packed { logic reset_b; field_t [3:0] ctrl; } hwif_t;
hwif_t hwif;

always_comb hwif = '0;

for (genvar slot = 0; slot < 4; slot++) begin : g
  initial force hwif.ctrl[slot].we = 1'b1;
end

typedef struct { logic we; logic [7:0] next; } entry_t;
entry_t entries [4];
initial force entries[2].we = 1'b1;
```

10.6.2: the target "can be a reference to a singular variable, a net, a constant bit-select of a
vector net, a constant part-select of a vector net, or a concatenation of these. It shall not be a
bit-select or a part-select of a variable". 7.2.1: a packed structure is "a mechanism for
subdividing a vector into subfields", so a member of `hwif` is bits of one vector variable. A member
of an unpacked structure and an element of an unpacked array are named nowhere in the list, which is
read as complete. Forbidden.

All three conditions hold. The front end keeps the refusal of every one of these as one warning
raised to an error, in the set it relaxes because "some tools violate the standard", and an open
simulator takes the text, forcing bits of a variable and an element of an unpacked array alike. No
allowed text says the same: forcing the whole variable restates every part the design drives, and
forcing a net adds one to the design for the testbench's sake.

The meaning Lyra gives it is the standard's rules for forcing a whole variable, applied to each
forced position on its own, where a value's type numbers its positions part by part: an integral
value's bits, a structure's members and an array's elements one after another. A procedural
assignment does not change a forced position while the force is in effect, and changes the positions
nobody forced as it would without one. After the release a position keeps the forced value until the
next procedural assignment, unless a continuous assignment or an `assign` drives the variable, which
is then reestablished there and reevaluated. A port that stands for what its connection drives goes
on showing that wherever nothing is forced. This is a choice, not a reading the standard forces: the
open simulator, which keeps the forced value of a whole variable as 10.6.2 says, takes the value
`always_comb` last wrote after the release of a part. A member of an unpacked union, one storage its
members read differently (7.3), and a part of a string, a queue, a dynamic array or an associative
array, whose parts lie at no fixed place in the value (7.10.3), are not yet carried out.

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

#### An input port of a 2-state or non-integral type with no port kind

```systemverilog
module leaf(input int count, input real level, input string name);
endmodule
```

23.2.2.3: "If the port kind is omitted: For input and inout ports, the port shall default to a net
of default net type." 6.7.1: "A valid data type for a net shall be one of the following: a) A
4-state integral type ... b) A fixed-size unpacked array or unpacked structure or union, where each
element has a valid data type for a net." `int` is 2-state and `real` and `string` are not integral,
so each port above is a net of a type no net may have. Forbidden. The clause's own examples write
the variable port `input var integer x`.

The text is in common use, and the front end accepts it with no diagnostic and takes each such port
as a variable, which is the only meaning it could have; Lyra keeps that. Write
`input var int count`, which every reading takes as that variable.

The front end takes `input logic` as a variable as well, which is not a forbidden text but an
analysis that differs from the standard's: `input logic d` is `input wire logic d`, a net, so left
unconnected it reads z (6.6) where a variable reads x. That is a defect of the analysis, recorded as
a wrong answer of Lyra's.

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

### Not settled by the standard, accepted by the front end

Run as the front end analyzed it. A tool that refuses the text is not wrong either, so no case may
claim either answer of every tool.

#### An event control on a chandle

```systemverilog
chandle watched;
initial @(watched) $display("changed");
```

6.14: "Chandles shall not be used as follows: ... In sensitivity lists or event expressions". 9.4.2:
"If the event expression is a reference to a simple object handle or chandle variable, an event is
created when a write to that variable is not equal to its previous value." One clause forbids the
text and the other gives it a meaning, and nothing says which governs.

The front end accepts it, and Lyra gives it the meaning 9.4.2 states: the wait ends at a write of a
different pointer, null included, and not at a write of the pointer the variable already holds.

#### A parameter default that no instance takes and that could not be evaluated

```systemverilog
module leaf #(parameter int W = 8, parameter int NEVER = W(1)) ();
endmodule
module top;
  leaf #(.NEVER(0)) u ();
endmodule
```

6.20.1 gives a parameter its default where the instantiation assigns none, and 23.10 has an
assignment override it. `W(1)` calls a parameter as though it were a function, which is no constant
expression. No clause says whether a default every instance overrides has to be valid all the same.

The front end evaluates a default only for an instance left to it, and Lyra keeps that, so the text
stops nothing while every instance assigns the parameter.

#### An unpacked array or structure put into a mailbox

```systemverilog
mailbox box = new;
int words [2] = '{3, 4};
initial box.put(words);
```

15.4.3 to 15.4.8 each say "The message can be any singular expression", which an unpacked array or
an unpacked structure is not (6.4), and none says whether another expression is an error.

The front end accepts it, and Lyra carries the message as it does any value: it keeps its type, and
it is taken only into a variable of an equivalent type (6.22.2).

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

### Allowed, with a meaning the standard does not settle

#### `$inferred_disable` in a property declared in another scope

```systemverilog
module m (input logic clk, rst_m, rst_g);
  default disable iff rst_m;
  property never(r = $inferred_disable);
    @(posedge clk) disable iff (r) 1'b0;
  endproperty
  property outer;
    never;
  endproperty
  if (1) begin : g
    default disable iff rst_g;
    a1: assert property (never);
    a2: assert property (outer);
  end
endmodule
```

16.14.7 defines the value twice. "The inferred disable condition is the disable condition from the
default disable iff declaration whose scope includes the call to `$inferred_disable`" -- read
literally, the call stands in the declaration's formal list, which is `m`'s, so `rst_m`. "An
inferred clocking or disable function call is replaced by the inferred expression as determined at
the point where the property, sequence, or checker is instantiated" -- for `a1` the instance stands
in `g`, so `rst_g`; for `a2` the instance of `never` stands inside `outer`, which is declared in
`m`, so the same sentence read the same way gives `rst_m` there. The front end hands the call on
unresolved, so its representation settles none of them either, and the clause's example declares and
asserts in one scope, which separates none of the readings.

Lyra takes the default in force where the assertion stands, however deep the instance, which is
`rst_g` for both `a1` and `a2`. It is the scope at which every other part of the assertion the
source leaves unwritten is resolved -- its clock (16.9.3, 16.16) and its own default disable
condition (16.15) -- so one lookup answers all of them (consequence 4). It is also the reading the
clause gives `$inferred_clock`, whose value for a nested instance is the clock flow "up to the
instance location in the property expression", that is, from the assertion; and it is what a
property written for others to assert means by taking its disable condition as a default. A design
meaning the declaration's own default writes it as the formal's default instead of
`$inferred_disable`.

#### A randomization hook named through an interface class handle

```systemverilog
interface class I;
endclass
class C implements I;
  int n;
  function void post_randomize();
    n = 1;
  endfunction
endclass
// C c = new; I h = c; h.post_randomize();   -- c.n is 1, or stays 0?
```

8.26.9 says "Interface classes contain two built-in empty virtual methods pre_randomize() and
post_randomize() that are automatically called before and after randomization. These methods can be
overridden. As a special case, pre_randomize() and post_randomize() shall not cause method name
conflicts." Read on its own, only an implementing class can override them, since an interface class
holds no body, and every class contains its own hooks (18.6.2), so the call runs `C`'s. But 18.6.2
also says "The pre_randomize() and post_randomize() methods are not virtual", and 8.26.2 says "An
inherited non-virtual method does not provide an implementation for a method of an implemented
interface class". Read with those, nothing in `C` implements `I`'s hook, and the call runs the empty
one. No committee record settles which.

Lyra runs the hook of the object's class, its own or one it inherits, which is the body
`randomize()` calls on that object. Each object then has one hook of each kind, whichever handle
reaches it, and a call through an interface class's handle is decided by the object as every other
method that handle reaches is (consequence 4). It is also what the clause's "automatically called
before and after randomization" describes, since the hook `randomize()` calls through `h` is `C`'s.
A design meaning the empty body has nothing to call.

#### A random state method named through an interface class handle

```systemverilog
interface class I;
endclass
class C implements I;
  rand int v;
endclass
// C c = new; I h = c; h.srandom(5); h.set_randstate(h.get_randstate());
```

8.26.9 makes `randomize()` and an inline constraint legal through an interface class handle, and
says "Use of rand_mode and constraint_mode shall not be legal as a consequence of the name
resolution rules and the fact that interface classes are not permitted to contain data members". It
says nothing of `srandom()`, `get_randstate()` and `set_randstate()` (18.13.3 to 18.13.5), which an
interface class does not declare either, so the name resolution argument it gives for the mode
methods reads on them too, while the reason it gives, that an interface class has no data members,
does not.

The front end accepts the calls, and Lyra acts on the generator of the object the handle refers to,
which is the only generator the call can reach.

#### The default of an override called through a base class handle

```systemverilog
class Base;
  virtual function int f(int x = 1); return x; endfunction
endclass
class Derived extends Base;
  virtual function int f(int x = 2); return x; endfunction
endclass
// Base h; Derived d = new; h = d; h.f()   -- 1, or 2?
```

8.20: "It is not necessary to have matching default expressions, but the presence of a default shall
match." 13.5.3 evaluates a default "in the scope containing the subroutine declaration" each time a
call omits the argument. Neither says whose declaration a virtual call omitting an argument takes,
the method the call names or the override that runs.

Lyra takes the default of the method the call names, that of the handle's class, and runs the
override's body with it, so `h.f()` is 1. A default is part of what the caller passes, and which
method the call names is known where it is written, while the override that runs is known only from
the object; C++ decides the same way for the same reason. Through `d`, both readings give 2.

#### A `try_get` while a `get` is waiting for more keys

```systemverilog
semaphore s = new(0);
initial s.get(2);
initial #1 begin
  s.put(1);
  $display(s.try_get(1));   // 1, or 0?
end
```

15.3.4: "If the specified number of required keys is less than or equal to the number of available
keys, the number of available keys is reduced by the specified keyCount, the method returns a
positive integer". 15.3.3: "The semaphore waiting queue is first-in first-out (FIFO). ... their
arrival order shall be preserved by the semaphore." Read alone, the first has the `try_get` take the
key; read with the second, the key is held for the process already waiting.

Lyra serves the waiting queue first, so the `try_get` answers 0 and the waiter takes its keys once
enough are returned. A `try_get` that took keys a waiter is owed would let a caller that never waits
pass every caller that does, which is the order 15.3.3 says the semaphore preserves.

#### Which bits `$fread` drops from a word that is not a whole number of bytes

```systemverilog
bit [8:0] nine [0:0];
// the file holds 8'h80, 8'h00
initial void'($fread(nine, fd));   // 9'h100, or 9'h000?
```

21.3.4.4: "a 9-bit wide memory is loaded using 2 bytes per memory word. The data are read from the
file in a big endian manner; the first byte read is used to fill the most significant location in
the memory element. If the memory width is not evenly divisible by 8 (8, 16, 24, 32), not all data
in the file are loaded into memory because of truncation." It does not say which end of the bytes
read the truncation drops.

Lyra places the bytes from the top of the word, as the first byte read fills the most significant
location, and drops the low bits of the last byte, so the word above is `9'h100`. A packed variable
read with `$fread` is taken the same way.

#### The end event of a block left by `return`, `break` or `continue`

```systemverilog
task automatic t(bit early);
  begin : body
    if (early) return;
    $display("ran on");
  end
endtask
covergroup cg @@(end t.body);
  ...
endgroup
```

19.3: an end block event "shall be triggered immediately after the corresponding block, task,
function, or method executes its last statement", and is not triggered "if the block, task,
function, or method is disabled". A `return`, a `break` or a `continue` that leaves the block is
neither its textually last statement nor a `disable`, and the clause says nothing of it.

Lyra takes the jump as the last statement the block executes, so the block ends there and the event
samples. Only a `disable` is named as leaving a block without ending it, and a jump out of a block
completes the block for every other purpose.

#### What `svDimensions` counts for a formal whose only open dimension is packed

```systemverilog
import "DPI-C" function int dimensions_of(input logic [] v);
// int dimensions_of(const svOpenArrayHandle h) { return svDimensions(h); }
```

Annex H.12.2: the query functions "use the same semantics" as the array queries (20.7), and "If the
dimension is 0, then the query refers to the packed part (which is one-dimensional) of an array, and
dimensions > 0 refer to the unpacked part". `$dimensions` counts every dimension, packed and
unpacked, which is 1 here; numbered as the functions number them, the formal has no unpacked
dimension, which is 0.

Lyra answers the number of unpacked dimensions, 0 here, so that `svDimensions` is the highest
dimension the other functions take for that handle.

#### When the right-hand side of an intra-assignment delay is read

```systemverilog
int a = 5;
function automatic int delay(); a = 99; return 2; endfunction
initial b = #(delay()) a;   // 5, or 99?
```

9.4.5: the right-hand side is evaluated "before the delay". Whether that means before the delay
expression is evaluated or only before the time it names elapses the standard does not say, and
Table 9-3 shows only constant delays.

Lyra reads the right-hand side first, and for a nonblocking assignment the left-hand side's index as
well, then evaluates the delay expression, so a delay expression that writes what the statement
reads reaches neither; `b` above is 5.

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
