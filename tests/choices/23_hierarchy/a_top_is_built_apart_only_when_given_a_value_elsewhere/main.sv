// @top: Top Counter Scaled
//
// A top-level module is one no instantiation names, implicitly instantiated
// once (LRM 23.3.1); the standard says nothing of a tool asked to make a top of
// a module the source also instantiates. Lyra builds it as asked, so the
// module stands twice: once as a top, which nothing hands a value, and once
// where `Top` instantiates it. A top whose parameters keep their defaults is
// built as an instance left to its defaults is; one given a value elsewhere,
// here by a defparam (LRM 23.10.1), keeps that value though nothing hands it
// over, beside an instance handed the same value. Each reads its parameter as
// a value and computes its total.
module Counter #(parameter int STEP = 3);
  int total;

  initial total = STEP * 2;
endmodule

module Scaled #(parameter int STEP = 3);
  int total;

  initial total = STEP * 2;
endmodule

module Top;
  Counter c ();
  Scaled #(.STEP(5)) s ();

  defparam Scaled.STEP = 5;

  final begin
    if (c.total !== 6) $fatal(1, "c.total was %0d, expected 6", c.total);
    if (Counter.total !== 6)
      $fatal(1, "Counter.total was %0d, expected 6", Counter.total);
    if (s.total !== 10) $fatal(1, "s.total was %0d, expected 10", s.total);
    if (Scaled.total !== 10)
      $fatal(1, "Scaled.total was %0d, expected 10", Scaled.total);
    $display("All checks passed");
  end
endmodule
