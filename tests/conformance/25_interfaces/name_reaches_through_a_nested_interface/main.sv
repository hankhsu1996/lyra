// An interface may instantiate another interface (LRM 25.3), and a port bound
// to the outer one reaches the inner instance through it: the name continues
// past the port into what that interface owns, reaching the inner instance's
// members and enabling its subroutines (LRM 25.10). What it reaches is the
// inner instance of whichever outer one the port was bound to, so two modules
// bound to different outer instances leave different storage changed. An inner
// declaration standing for several instances is selected the way any interface
// array is, a name reached this way re-triggers a process that reads it, and
// the instance it names connects to a deeper module's own interface port.
interface Inner;
  int hits;
  logic [7:0] mark;

  function automatic void Bump(input logic [7:0] value);
    hits = hits + 1;
    mark = value;
  endfunction
endinterface

interface Outer;
  Inner inner ();
  Inner bank[2] ();
endinterface

module Counted (
    Inner i
);
  initial #3 i.Bump(8'h77);
endmodule

module Leaf (
    Outer o
);
  logic [7:0] seen = 8'h00;
  logic [7:0] woke = 8'h00;

  Counted onward (o.inner);

  initial #1 o.inner.Bump(8'h5a);
  initial #2 seen = o.inner.mark;
  initial #1 begin
    o.bank[0].mark = 8'h11;
    o.bank[1].mark = 8'h33;
  end

  always @(o.inner.mark) woke = o.inner.mark;
endmodule

module Top;
  Outer first ();
  Outer second ();

  Leaf on_first (first);
  Leaf on_second (second);

  initial #1 second.inner.mark = 8'ha5;

  final begin
    if (first.inner.hits !== 2)
      $fatal(1, "first.inner.hits was %0d, expected 2", first.inner.hits);
    if (second.inner.hits !== 2)
      $fatal(1, "second.inner.hits was %0d, expected 2", second.inner.hits);
    if (first.inner.mark !== 8'h77)
      $fatal(1, "first.inner.mark was %h, expected 77", first.inner.mark);
    if (on_first.seen !== 8'h5a)
      $fatal(1, "on_first.seen was %h, expected 5a", on_first.seen);
    if (on_first.woke !== 8'h77)
      $fatal(1, "on_first.woke was %h, expected 77", on_first.woke);
    if (first.bank[0].mark !== 8'h11)
      $fatal(1, "first.bank[0].mark was %h, expected 11", first.bank[0].mark);
    if (first.bank[1].mark !== 8'h33)
      $fatal(1, "first.bank[1].mark was %h, expected 33", first.bank[1].mark);
    $display("All checks passed");
  end
endmodule
