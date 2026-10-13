// A class a module declares is a type of each instance (LRM 8.25), and the
// same declaration in every one. Instances that select different alternatives
// of a conditional generate (LRM 27.5), or build different numbers of blocks
// of a loop generate each declaring a class (LRM 27.4), each use the class
// they hold, and each object keeps its own values. A constant a method of the
// class computes from the module's parameter is each instance's own.
module Declares #(parameter int P = 0) (output int o);
  class Counter;
    int n;
    function int twice();
      return n * 2;
    endfunction
  endclass
  Counter c = new;
  if (P != 0) begin : on
    initial begin
      c.n = P;
      o = c.twice();
    end
  end else begin : off
    class Extra;
      int k = 7;
    endclass
    Extra e = new;
    initial begin
      c.n = 2;
      o = c.twice() + e.k;
    end
  end
endmodule

module Counted #(parameter int N = 1) (output int o);
  for (genvar i = 0; i < N; i++) begin : g
    class Cell;
      int v;
    endclass
    Cell c = new;
    initial c.v = i * 10;
  end
  initial #1 o = g[N-1].c.v;
endmodule

module Scaled #(parameter int P = 0) (output int o);
  class Scale;
    function int get();
      localparam int L = P * 2;
      return L;
    endfunction
  endclass
  Scale s = new;
  initial o = s.get();
endmodule

module Top;
  int d[3];
  int n[2];
  int s[2];
  Declares #(.P(0)) declares_off (.o(d[0]));
  Declares #(.P(1)) declares_on (.o(d[1]));
  Declares #(.P(3)) declares_on_again (.o(d[2]));
  Counted #(.N(1)) one_block (.o(n[0]));
  Counted #(.N(3)) three_blocks (.o(n[1]));
  Scaled #(.P(1)) scaled_one (.o(s[0]));
  Scaled #(.P(2)) scaled_two (.o(s[1]));

  final begin
    if (d[0] !== 11) $fatal(1, "declares_off gave %0d", d[0]);
    if (d[1] !== 2) $fatal(1, "declares_on gave %0d", d[1]);
    if (d[2] !== 6) $fatal(1, "declares_on_again gave %0d", d[2]);
    if (n[0] !== 0) $fatal(1, "one_block gave %0d", n[0]);
    if (n[1] !== 20) $fatal(1, "three_blocks gave %0d", n[1]);
    if (s[0] !== 2) $fatal(1, "scaled_one gave %0d", s[0]);
    if (s[1] !== 4) $fatal(1, "scaled_two gave %0d", s[1]);
    $display("All checks passed");
  end
endmodule
