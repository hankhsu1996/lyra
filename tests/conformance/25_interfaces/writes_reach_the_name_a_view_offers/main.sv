// A name a modport offers is written like any other name that reaches storage.
// A simple port identifier is used as both a reference to an interface item and
// a port identifier (LRM 25.5.4), so it denotes that item and nothing about the
// view changes how it is reached. A port identifier that carries an expression
// denotes what that expression denotes, and where the view admits a write the
// expression must resolve to a legal expression for the type of module port
// (LRM 25.5.4, 23.3.3) -- so it designates storage, whether that is a part of
// one declaration or a concatenation of several. Every assignment form the
// language allows for such storage therefore reaches it: blocking, compound and
// increment (LRM 11.4.1), a part-select target, an intra-assignment delay and a
// nonblocking update whose left-hand side is evaluated when the statement runs
// (LRM 10.4.2), a continuous assignment (LRM 10.3), and a procedural continuous
// assignment (LRM 10.6.2). A port identifier the view offers only for reading
// may name any expression over the interface's declarations, and reading it is
// the interface evaluating that expression.
interface Bus;
  logic [7:0] r = 8'h00;
  logic [3:0] hi, lo;
  logic [7:0] whole;
  int         counter;
  logic [7:0] driven;
  logic [7:0] held;
  logic [7:0] lent;
  wire  [7:0] wired;

  modport ctrl(
      output counter,
      output whole,
      output driven,
      output held,
      output .Part(r[3:0]),
      output .Pair({hi, lo}),
      output .Half(wired[7:4]),
      ref lent,
      input .Doubled(counter * 2)
  );
endinterface

module Driver (
    Bus.ctrl p
);
  int dbl;

  // A continuous assignment drives a name the view offers, whether the view
  // named the item itself or part of one.
  assign p.driven = 8'hC3;
  assign p.Half   = 4'h9;

  // Reading a name offered only for reading re-evaluates the interface's own
  // expression whenever what it reads changes.
  always_comb dbl = p.Doubled;

  initial begin
    // Blocking, compound, and increment all reach the one storage.
    p.counter = 10;
    p.counter += 5;
    p.counter++;

    // A part of a name the view offers is a target on its own.
    p.whole = 8'hF0;
    p.whole[3:0] = 4'h5;

    // A renamed identifier reaches exactly the part the view named, and a
    // renamed concatenation reaches each of its parts. Both are storage the
    // view designated, so the same forms reach them.
    p.Part  = 4'h8;
    p.Part += 4'h2;
    p.Pair <= 8'h3C;

    // A name the view offers by reference is writable.
    p.lent = 8'h77;

    // The update lands at the end of this time step.
    p.counter <= 99;

    // A procedural continuous assignment takes the name over.
    force p.held = 8'hFF;

    // The right-hand side is evaluated now and the assignment happens after
    // the delay, so this lands at time 4 rather than at time 2.
    #2 p.counter = #2 42;
  end
endmodule

module Top;
  Bus n ();
  Driver d (.p(n.ctrl));

  initial begin
    #1;
    if (n.counter !== 99)
      $fatal(1, "a nonblocking update did not reach the name, counter was %0d", n.counter);
    if (n.whole !== 8'hF5)
      $fatal(1, "a part-select target did not reach the name, whole was %h", n.whole);
    if (n.r !== 8'h0A) $fatal(1, "a renamed part was not written, r was %h", n.r);
    if (n.hi !== 4'h3 || n.lo !== 4'hC)
      $fatal(1, "a renamed concatenation was not written, hi %h lo %h", n.hi, n.lo);
    if (n.wired[7:4] !== 4'h9)
      $fatal(1, "a continuous assignment did not drive a renamed part, saw %h", n.wired[7:4]);
    if (n.lent !== 8'h77)
      $fatal(1, "a name offered by reference was not written, lent was %h", n.lent);
    if (n.driven !== 8'hC3)
      $fatal(1, "a continuous assignment did not drive the name, driven was %h", n.driven);
    if (n.held !== 8'hFF)
      $fatal(1, "force did not take the name over, held was %h", n.held);
    if (d.dbl !== 198)
      $fatal(1, "a name offered for reading evaluated to %0d, expected 198", d.dbl);

    #4;
    if (n.counter !== 42)
      $fatal(1, "an intra-assignment delay did not reach the name, counter was %0d", n.counter);
    if (d.dbl !== 84)
      $fatal(1, "a name offered for reading did not re-evaluate, saw %0d", d.dbl);
  end

  final $display("All checks passed");
endmodule
