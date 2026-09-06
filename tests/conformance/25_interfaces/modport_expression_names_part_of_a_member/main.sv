// A modport expression gives a port identifier its own meaning inside the
// interface (LRM 25.5.4): the identifier names an element, a part-select, a
// concatenation, an assignment pattern, or a constant built from what the
// interface declared, and a module's access through that identifier reaches
// exactly that. Port identifiers live in each modport's own name space, so one
// name carries a different meaning per modport, and a module written once
// against the name acts on whichever part the modport it was bound through
// named. A port expression is self-determined and is not an assignment-like
// context, and it is optional, so a port may connect to nothing internal at
// all. Reading such a name is the interface evaluating that expression, so a
// process sensitive to the name re-evaluates whenever any storage the
// expression reads changes.
interface Nibbles;
  logic [7:0] r;
  const int   one = 1;
  bit         flag;

  modport low(output .Part(r[3:0]), input .Value(one), flag);
  modport high(output .Part(r[7:4]), input .Value(2), flag);
  modport watch(input .Doubled(r + r), .Whole(r));
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

// What a name a modport offers stands for is an expression, so a process
// waiting on it waits on what that expression reads rather than on a signal of
// its own -- a computed name and a plain one alike.
module Watcher (
    interface i
);
  logic [8:0] doubled;
  logic [7:0] whole;

  always_comb doubled = i.Doubled;
  always_comb whole = i.Whole;
endmodule

module Top;
  Nibbles n ();

  Writer low (.i(n.low));
  Writer high (.i(n.high));
  Watcher w (.i(n.watch));

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
    $display("All checks passed");
  end
endmodule
