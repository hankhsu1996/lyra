// A port connection is a continuous assignment of source to sink (LRM 23.3.3).
// For an input port declared with a variable data type the assignment runs
// from the connection expression into the port, and for an output port it runs
// from the port out to what the port is connected to (LRM 23.3.3.2). Being
// continuous, each one re-evaluates whenever its source changes rather than
// sampling once. The connection may be written by position or by name
// (LRM 23.3.2.1, 23.3.2.2) and may be any expression, including one that names
// no variable at all, and a port of an enclosing module may itself serve as
// the connection for a port one level further down.
module Sink(input int a);
  int captured;

  always_comb captured = a;
endmodule

module Adder(input int a, output int b);
  always_comb b = a + 1;
endmodule

module Counter(input bit clk, output int count);
  always_ff @(posedge clk) count <= count + 1;
endmodule

module Inner(input int x);
  int doubled;

  always_comb doubled = x * 2;
endmodule

module Outer(input int a);
  Inner inner(.x(a));
endmodule

module Top;
  int p;
  int q;
  Sink named(.a(p + q));
  Sink positional(q);
  Sink from_literal(.a(42));

  int src;
  int incremented;
  Adder u(.a(src), .b(incremented));

  bit clk;
  int cnt;
  Counter ctr(.clk(clk), .count(cnt));

  int forwarded;
  Outer outer(.a(forwarded));

  int named_at_1;
  int positional_at_1;

  initial begin
    p = 5;
    q = 3;
    src = 5;
    forwarded = 21;
    clk = 0;
    #1;
    named_at_1 = named.captured;
    positional_at_1 = positional.captured;
    q = 7;
    clk = 1;
    #1 clk = 0;
    #1 clk = 1;
    #1;
  end

  final begin
    if (named_at_1 !== 8)
      $fatal(1, "named_at_1 was %0d, expected 8", named_at_1);
    if (positional_at_1 !== 3)
      $fatal(1, "positional_at_1 was %0d, expected 3", positional_at_1);
    if (named.captured !== 12)
      $fatal(1, "named.captured was %0d, expected 12", named.captured);
    if (positional.captured !== 7)
      $fatal(1, "positional.captured was %0d, expected 7",
             positional.captured);
    if (from_literal.captured !== 42)
      $fatal(1, "from_literal.captured was %0d, expected 42",
             from_literal.captured);
    if (incremented !== 6)
      $fatal(1, "incremented was %0d, expected 6", incremented);
    if (cnt !== 2) $fatal(1, "cnt was %0d, expected 2", cnt);
    if (outer.inner.doubled !== 42)
      $fatal(1, "outer.inner.doubled was %0d, expected 42",
             outer.inner.doubled);
    $display("All checks passed");
  end
endmodule
