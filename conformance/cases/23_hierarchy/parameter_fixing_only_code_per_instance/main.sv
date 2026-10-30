// A parameter that only what a module's statements run requires a constant of
// -- the width a process selects, a replication a function returns -- still
// holds each instance's own value (LRM 23.10), however alike the instances
// look from outside. A subroutine called through each instance's name runs
// that instance's code (LRM 23.6), a value handed on to a child does the same
// one level down, and the parameter read as a value reads the instance's own.
// A structure the module declares is one type of the module (LRM 7.2), built
// and compared alike in every instance whatever its code was compiled for.
// Where one instance's code needs no constant of the parameter and another's
// does -- a part select only one value reaches out of range (LRM 11.5.1) --
// each still runs its own, a forked process included.
module Window #(parameter int K = 0) (input logic [7:0] v, output logic [7:0] w,
                                      output int forked);
  assign w = v[K+:8];

  initial fork
    forked = K + 40;
  join_none
endmodule

module Leaf #(parameter int P = 1) (input logic [7:0] in, output logic [7:0] out);
  always_comb out = 8'(in[P:0]);

  function automatic logic [7:0] ones();
    return 8'({(P + 1){1'b1}});
  endfunction

  function automatic int plus(int x);
    return x + P;
  endfunction

  typedef struct {
    logic [7:0] ones;
    int plus;
  } pair_t;

  function automatic pair_t pair();
    pair_t made;
    made.ones = ones();
    made.plus = plus(0);
    return made;
  endfunction

  function automatic bit pair_holds();
    pair_t fresh;
    return pair() == pair() && fresh.plus == 0 && pair() !== fresh;
  endfunction
endmodule

module Mid #(parameter int K = 1) (input logic [7:0] in, output logic [7:0] out);
  Leaf #(.P(K)) leaf (.in(in), .out(out));
endmodule

module Top;
  logic [7:0] in = 8'hff;
  logic [7:0] narrow_out, wide_out;
  logic [7:0] mid_out [2];
  logic [7:0] in_range, past_the_end;
  int in_range_forked, past_the_end_forked;

  Leaf #(.P(1)) narrow (.in(in), .out(narrow_out));
  Leaf #(.P(3)) wide (.in(in), .out(wide_out));
  Window #(.K(0)) fits (.v(8'ha5), .w(in_range), .forked(in_range_forked));
  Window #(.K(1)) overhangs (.v(8'ha5), .w(past_the_end),
                             .forked(past_the_end_forked));

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
    if (narrow.pair().plus !== 1 || wide.pair().plus !== 3)
      $fatal(1, "the pairs hold %0d and %0d", narrow.pair().plus,
             wide.pair().plus);
    if (!narrow.pair_holds() || !wide.pair_holds())
      $fatal(1, "a pair is not built or compared alike");
    if (mid_out[0] !== 8'h07) $fatal(1, "g[0] selected %h", mid_out[0]);
    if (mid_out[1] !== 8'h0f) $fatal(1, "g[1] selected %h", mid_out[1]);
    if (g[0].mid.leaf.ones() !== 8'h07)
      $fatal(1, "g[0].mid.leaf.ones() is %h", g[0].mid.leaf.ones());
    if (g[1].mid.leaf.ones() !== 8'h0f)
      $fatal(1, "g[1].mid.leaf.ones() is %h", g[1].mid.leaf.ones());
    if (in_range !== 8'ha5) $fatal(1, "the select in range read %h", in_range);
    if (past_the_end !== 8'bx1010010)
      $fatal(1, "the select past the end read %b", past_the_end);
    if (in_range_forked !== 40 || past_the_end_forked !== 41)
      $fatal(1, "the forked processes wrote %0d and %0d", in_range_forked,
             past_the_end_forked);
    $display("All checks passed");
  end
endmodule
