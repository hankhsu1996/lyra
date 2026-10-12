# Language Conformance

What source text Lyra accepts, and how a text nobody is sure about gets ruled on.

## The rule

**Lyra reads IEEE 1800-2023. A text the standard forbids is an error where it is written; a text the
standard allows is accepted and does what the standard says.**

Nothing is accepted because another tool accepts it. Every simulator takes some text the standard
forbids, no two take the same set, and the ones that take a forbidden text do not agree on what it
means -- so there is no tolerant reading to adopt, only one tool's choice to copy. A design accepted
that way means what that tool decided, and nothing tells its author.

Four consequences, each of which has been argued the other way and lost:

1. **A text that is widely taken is still an error.** An exception for "every tool takes this one"
   turns the rule into "the standard, except where enough tools agree", and that boundary cannot be
   stated in advance. The first exception is what the second is argued from.
2. **A text the standard does not settle is read the stricter way.** A design refused can be
   rewritten in a form every reading accepts. A design accepted under one reading of an unclear
   clause means something else under the other.
3. **What is legal is the front end's to answer, and Lyra keeps no list of its own.** The front
   end's default reading is the reading Lyra ships. Where that default is wrong in either direction
   -- it takes a forbidden text, or refuses an allowed one -- the front end is corrected, upstream,
   and Lyra follows. A table in Lyra of diagnostics to raise and lower would be a second statement
   of the standard, and every entry in it a disagreement that was never argued where it could be
   settled. **A choice the standard leaves to the tool is different**: there the front end is not
   wrong whichever way it went, so nothing can be corrected upstream, and Lyra makes the choice
   itself (see _Given a meaning, but a tool may refuse it_ below).
4. **A caller may ask for another tool's reading by name** (`--compat`, or one of the front end's
   options for a single reading, such as `--allow-hierarchical-const`). What the design then means
   is that tool's answer, not one Lyra stands behind.

## Ruling on a disputed text

A design stops, or a text is refused that its author expected to work, and somebody has to say whose
defect it is. The steps are in this order because each one has overturned a ruling made without it.

1. **Reduce it** to a few lines that show the construct alone.
2. **Read the clause that defines the construct** and quote the sentence.
3. **Search for the clause that would make the other side right** before saying anyone is wrong. Two
   clauses can each read as the whole answer (see _One module described twice_ below).
4. **Check the reduction writes what the design writes.** A reduction that reaches a name through an
   instance, where the design reaches it through an interface port, rules on another construct.
5. **Rule**: the design is wrong, the front end is wrong, Lyra is wrong, or the standard does not
   settle it.

The ruling is argued from the standard's text and from nothing else. What an open-source tool does
with the reduction may be noted beside it, as a reason the text went unnoticed or as a second reader
of the clause, and its source can be read for the clause it relies on. It is never the reason for a
ruling: a tool that accepts says only that it accepts.

A design that is wrong is reported to whoever maintains it. A front end that is wrong is reported
upstream. Neither is answered by changing what Lyra accepts.

## The rulings

Each text below was met in a real design or in Lyra's own tests and was disputed: some tool takes
it, or some tool refuses it, against what one would expect.

### Forbidden

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
obvious meaning to give it. It is an error all the same (consequence 1). Write `mubi_t'(j)`.

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
member of an element is "a singular variable" is a reading, and the list is read as complete.

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

- **A setting in Lyra that makes a forbidden text legal by default.** The reason is consequence 3:
  it is a second statement of the standard that nobody argues with the front end.
- **A ruling whose stated reason is that a tool accepts or refuses the text.** A tool is at most a
  second reader of the clause, and tools that take a forbidden text do not agree on what it means.
- **A carried change to somebody else's design that only suits Lyra.** A design that is wrong is
  wrong for every tool that follows the standard, and the correction is one its maintainer can take.
