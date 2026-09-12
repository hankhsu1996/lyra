// A modport expression gives a port identifier its own meaning inside the
// interface (LRM 25.5.4): the identifier names an element, a part-select, a
// concatenation, an assignment pattern, or a constant built from what the
// interface declared, and a module's access through that identifier reaches
// exactly that. Port identifiers live in each modport's own name space, so one
// name carries a different meaning per modport, and a module written once
// against the name acts on whichever part the modport it was bound through
// named. A port expression is self-determined and is not an assignment-like
// context, and it is optional (`.Nowhere()` below), so a port may connect to
// nothing internal at all and a view declaring one is still a legal interface.
// A name standing for an expression re-evaluates whenever any storage that
// expression reads changes.
interface Nibbles;
  logic [7:0] r;
  const int   one = 1;
  bit         flag;

  modport low(output .Part(r[3:0]), input .Value(one), flag);
  modport high(output .Part(r[7:4]), input .Value(2), flag);
  modport watch(input .Doubled(r + r), .Whole(r), output .Nowhere());
endinterface

module Writer (
    interface i
);
  bit saw_flag = 1'b0;

  initial #1 begin
    i.Part  = i.Value;
    saw_flag = i.flag;
  end
endmodule

// A name the view bound to an expression has no storage of its own, so what a
// process waiting on it observes is whatever that expression reads -- and it
// re-evaluates exactly when a name that is an interface item does.
module Watcher (
    interface i
);
  logic [8:0] doubled;
  logic [7:0] whole;

  always_comb doubled = i.Doubled;
  always_comb whole = i.Whole;
endmodule

// What a view offers is a property of the interface's declaration, so an
// interface port is one way to reach it and not the only one: a scope that
// declares the instance names the view on it directly, under as many views as
// it likes, and reading, writing, and waiting on those names work there exactly
// as they do through a port.
module Local;
  Nibbles own ();
  logic [8:0] doubled;

  always_comb doubled = own.watch.Doubled;

  initial begin
    own.r = 8'h00;
    #1 own.low.Part = own.low.Value;
    #2 own.high.Part = 4'h5;
  end
endmodule

module Top;
  Nibbles n ();

  Writer low (.i(n.low));
  Writer high (.i(n.high));
  Watcher w (.i(n.watch));
  Local loc ();

  initial n.flag = 1'b1;

  initial begin
    #2;
    if (w.doubled !== 9'h42) $fatal(1, "w.doubled was %h, expected 042", w.doubled);
    if (w.whole !== 8'h21) $fatal(1, "w.whole was %h, expected 21", w.whole);
    n.r = 8'h10;
    #1;
    if (w.doubled !== 9'h20)
      $fatal(1, "a computed name did not re-evaluate, saw %h", w.doubled);
    if (w.whole !== 8'h10)
      $fatal(1, "a plain name did not re-evaluate, saw %h", w.whole);
  end

  final begin
    // The two writers ran at time 1 and each wrote its own nibble of the one
    // declaration both views name.
    if (w.whole !== 8'h10) $fatal(1, "w.whole was %h, expected 10", w.whole);
    // A plain identifier beside the expression ports keeps its own meaning:
    // it names the interface item itself, under either modport.
    if (low.saw_flag !== 1'b1)
      $fatal(1, "low.saw_flag was %b, expected 1", low.saw_flag);
    if (high.saw_flag !== 1'b1)
      $fatal(1, "high.saw_flag was %b, expected 1", high.saw_flag);
    // The scope that declares the instance wrote each nibble through a
    // different view of it, and its own process saw the computed name move.
    if (loc.own.r !== 8'h51) $fatal(1, "loc.own.r was %h, expected 51", loc.own.r);
    if (loc.doubled !== 9'h0a2)
      $fatal(1, "loc.doubled was %h, expected 0a2", loc.doubled);
    $display("All checks passed");
  end
endmodule
