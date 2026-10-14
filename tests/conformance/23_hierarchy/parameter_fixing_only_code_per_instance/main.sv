// A parameter that only what a module's statements run requires a constant of
// -- the width a process selects, a replication a function returns -- still
// holds each instance's own value (LRM 23.10), however alike the instances
// look from outside. A subroutine called through each instance's name runs
// that instance's code (LRM 23.6), a value handed on to a child does the same
// one level down, and the parameter read as a value reads the instance's own.
module Leaf #(parameter int P = 1) (input logic [7:0] in, output logic [7:0] out);
  always_comb out = 8'(in[P:0]);

  function automatic logic [7:0] ones();
    return 8'({(P + 1){1'b1}});
  endfunction

  function automatic int plus(int x);
    return x + P;
  endfunction
endmodule

module Mid #(parameter int K = 1) (input logic [7:0] in, output logic [7:0] out);
  Leaf #(.P(K)) leaf (.in(in), .out(out));
endmodule

module Top;
  logic [7:0] in = 8'hff;
  logic [7:0] narrow_out, wide_out;
  logic [7:0] mid_out [2];

  Leaf #(.P(1)) narrow (.in(in), .out(narrow_out));
  Leaf #(.P(3)) wide (.in(in), .out(wide_out));

  for (genvar i = 0; i < 2; i++) begin : g
    Mid #(.K(i + 2)) mid (.in(in), .out(mid_out[i]));
  end

  initial begin
    #1;
    if (narrow_out !== 8'h03) $fatal(1, "narrow selected %h", narrow_out);
    if (wide_out !== 8'h0f) $fatal(1, "wide selected %h", wide_out);
    if (narrow.ones() !== 8'h03) $fatal(1, "narrow.ones() is %h", narrow.ones());
    if (wide.ones() !== 8'h0f) $fatal(1, "wide.ones() is %h", wide.ones());
    if (narrow.plus(10) !== 11) $fatal(1, "narrow.plus is %0d", narrow.plus(10));
    if (wide.plus(10) !== 13) $fatal(1, "wide.plus is %0d", wide.plus(10));
    if (mid_out[0] !== 8'h07) $fatal(1, "g[0] selected %h", mid_out[0]);
    if (mid_out[1] !== 8'h0f) $fatal(1, "g[1] selected %h", mid_out[1]);
    if (g[0].mid.leaf.ones() !== 8'h07)
      $fatal(1, "g[0].mid.leaf.ones() is %h", g[0].mid.leaf.ones());
    if (g[1].mid.leaf.ones() !== 8'h0f)
      $fatal(1, "g[1].mid.leaf.ones() is %h", g[1].mid.leaf.ones());
    $display("All checks passed");
  end
endmodule
