// An interface may instantiate another interface (LRM 25.3), and a port bound
// to the outer one reaches the inner instance through it: the name continues
// past the port into what that interface owns, reaching the inner instance's
// members and enabling its subroutines. What it reaches is the inner instance
// of whichever outer one the port was bound to, so two modules bound to
// different outer instances leave different storage changed.
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
endinterface

module Leaf (
    Outer o
);
  logic [7:0] seen = 8'h00;

  initial #1 o.inner.Bump(8'h5a);
  initial #2 seen = o.inner.mark;
endmodule

module Top;
  Outer first ();
  Outer second ();

  Leaf on_first (first);
  Leaf on_second (second);

  initial #1 second.inner.mark = 8'ha5;

  final begin
    if (first.inner.hits !== 1)
      $fatal(1, "first.inner.hits was %0d, expected 1", first.inner.hits);
    if (second.inner.hits !== 1)
      $fatal(1, "second.inner.hits was %0d, expected 1", second.inner.hits);
    if (on_first.seen !== 8'h5a)
      $fatal(1, "on_first.seen was %h, expected 5a", on_first.seen);
    $display("All checks passed");
  end
endmodule
