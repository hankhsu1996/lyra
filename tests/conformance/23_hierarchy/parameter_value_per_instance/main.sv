// Every instance of a module holds the parameter values its own instantiation
// gave it (LRM 23.10), wherever the module reads them: an expression, a
// constant derived from one -- at the module's level, or in a function, a
// sequential block or a parallel block -- another parameter's default, which
// way a procedural choice goes, a child's own parameter, a declared width (LRM
// 6.20.2), and which generate alternative stands (LRM 27.5). Instances of one
// module differing only in a value each see their own.
//
// Some positions take a constant the front end settles while binding, so the
// value decides what that position means: a size cast's width (LRM 6.24.1),
// the index of a hierarchical name (LRM 23.6), a streaming concatenation's
// slice size (LRM 11.4.14.2), a sequence's delay (LRM 16.7), and a bounded
// queue's bound (LRM 7.10).
module Inner #(parameter int V = 0) (output int o);
  initial o = V;
endmodule

module Leaf #(
    parameter int K = 0,
    parameter int W = 4,
    parameter int D = K + 100,
    parameter string NAME = "none",
    parameter real R = 0.5
) (
    output int got,
    output int derived,
    output int from_default,
    output int chosen,
    output int width,
    output int nested,
    output int name_len,
    output int real_scaled,
    output int in_function,
    output int in_block,
    output int in_fork
);
  localparam int L = K * 2 + 1;
  logic [W-1:0] w;

  Inner #(.V(K + 1)) inner (.o(nested));

  function automatic int Tripled();
    localparam int T = K * 3;
    return T;
  endfunction

  assign in_function = Tripled();

  initial begin
    got = K;
    derived = L;
    from_default = D;
    if (K > 2) chosen = 5;
    else chosen = 6;
    width = $bits(w);
    name_len = NAME.len();
    real_scaled = int'(R * 10.0);
    begin : blk
      localparam int P = K * 5;
      in_block = P;
    end
    fork : par
      localparam int F = K * 7;
      in_fork = F;
    join
  end
endmodule

// A value parameter beside one that sizes a declaration, for instances that
// differ only in the width.
module Sized #(parameter int K = 0, parameter int W = 4) (output int width);
  logic [W-1:0] w;
  initial width = $bits(w) + K;
endmodule

// A value wider than any machine word, deciding which alternative stands. Two
// instances whose values differ only in their lowest bits are two
// applications, however long the values are.
module Wide #(parameter logic [199:0] P = '0) (output int picked);
  if (P[0]) begin : odd
    assign picked = 1;
  end else begin : even
    assign picked = 2;
  end
endmodule

module Folded #(parameter int W = 4) (
    input logic clk,
    input logic start,
    input logic stop,
    output int cast_value,
    output int picked,
    output logic [15:0] streamed,
    output int queued,
    output int missed
);
  int x = 1000;
  logic [15:0] bits = 16'h1234;
  int q[$:W];

  for (genvar i = 0; i < 16; i++) begin : g
    int v;
    initial v = i * 10;
  end

  initial missed = 0;
  assert property (@(posedge clk) start |-> ##W stop) else missed = missed + 1;

  initial begin
    #1;
    cast_value = int'(W'(x));
    picked = g[W].v;
    streamed = {<< W {bits}};
    for (int k = 0; k < W + 2; k++) q.push_back(k);
    queued = q.size();
  end
endmodule

module Top;
  int got[1:4];
  int derived[1:4];
  int from_default[1:4];
  int chosen[1:4];
  int width[1:4];
  int nested[1:4];
  int name_len[1:4];
  int real_scaled[1:4];
  int in_function[1:4];
  int in_block[1:4];
  int in_fork[1:4];
  int narrow_width;
  int wide_width;
  int odd_picked;
  int even_picked;

  for (genvar i = 1; i <= 4; i++) begin : g
    Leaf #(
        .K(i),
        .NAME(i == 1 ? "a" : "abc"),
        .R(i * 1.5)
    ) u (
        .got(got[i]),
        .derived(derived[i]),
        .from_default(from_default[i]),
        .chosen(chosen[i]),
        .width(width[i]),
        .nested(nested[i]),
        .name_len(name_len[i]),
        .real_scaled(real_scaled[i]),
        .in_function(in_function[i]),
        .in_block(in_block[i]),
        .in_fork(in_fork[i])
    );
  end

  Sized #(.K(7), .W(2)) narrow (.width(narrow_width));
  Sized #(.K(7), .W(16)) wide (.width(wide_width));
  Wide #(.P({1'b1, 199'd1})) odd (.picked(odd_picked));
  Wide #(.P({1'b1, 199'd2})) even (.picked(even_picked));

  // Ticks land at 5, 15, 25, and so on. Sampled, `start` holds only at the
  // tick at 15 and `stop` only at the tick four later, at 55, so a delay of 4
  // finds it and a delay of 8 does not.
  logic clk = 0;
  logic start = 0, stop = 0;
  always #5 clk = ~clk;
  initial begin
    #8 start = 1;
    #10 start = 0;
    #30 stop = 1;
    #10 stop = 0;
    #62 $finish;
  end

  int cast4, cast8, picked4, picked8, queued4, queued8, missed4, missed8;
  logic [15:0] streamed4, streamed8;
  Folded #(.W(4)) f4 (
      clk, start, stop, cast4, picked4, streamed4, queued4, missed4
  );
  Folded #(.W(8)) f8 (
      clk, start, stop, cast8, picked8, streamed8, queued8, missed8
  );

  final begin
    for (int i = 1; i <= 4; i++) begin
      if (got[i] !== i) $fatal(1, "g[%0d] read K as %0d", i, got[i]);
      if (derived[i] !== 2 * i + 1)
        $fatal(1, "g[%0d] derived %0d from K", i, derived[i]);
      if (from_default[i] !== i + 100)
        $fatal(1, "g[%0d] defaulted D to %0d", i, from_default[i]);
      if (chosen[i] !== (i > 2 ? 5 : 6))
        $fatal(1, "g[%0d] chose %0d", i, chosen[i]);
      if (width[i] !== 4) $fatal(1, "g[%0d] declared %0d bits", i, width[i]);
      if (nested[i] !== i + 1)
        $fatal(1, "g[%0d] handed its child %0d", i, nested[i]);
      if (name_len[i] !== (i == 1 ? 1 : 3))
        $fatal(1, "g[%0d] read a name of length %0d", i, name_len[i]);
      if (real_scaled[i] !== i * 15)
        $fatal(1, "g[%0d] read R as %0d tenths", i, real_scaled[i]);
      if (in_function[i] !== 3 * i)
        $fatal(1, "g[%0d] function constant read %0d", i, in_function[i]);
      if (in_block[i] !== 5 * i)
        $fatal(1, "g[%0d] block constant read %0d", i, in_block[i]);
      if (in_fork[i] !== 7 * i)
        $fatal(1, "g[%0d] fork constant read %0d", i, in_fork[i]);
    end
    if (narrow_width !== 9) $fatal(1, "narrow answered %0d", narrow_width);
    if (wide_width !== 23) $fatal(1, "wide answered %0d", wide_width);
    if (odd_picked !== 1) $fatal(1, "odd picked %0d", odd_picked);
    if (even_picked !== 2) $fatal(1, "even picked %0d", even_picked);
    // 1000 is 'h3E8; a size cast keeps the operand's signedness, so its low 4
    // bits read as -8 and its low 8 bits as -24.
    if (cast4 !== -8 || cast8 !== -24)
      $fatal(1, "size casts gave %0d and %0d", cast4, cast8);
    if (picked4 !== 40 || picked8 !== 80)
      $fatal(1, "hierarchical indices reached %0d and %0d", picked4, picked8);
    if (streamed4 !== 16'h4321 || streamed8 !== 16'h3412)
      $fatal(1, "streams gave %h and %h", streamed4, streamed8);
    // A bound of W holds W + 1 elements; the push past it is discarded.
    if (queued4 !== 5 || queued8 !== 9)
      $fatal(1, "bounded queues held %0d and %0d", queued4, queued8);
    if (missed4 !== 0 || missed8 !== 1)
      $fatal(1, "delays of 4 and 8 missed %0d and %0d times", missed4, missed8);
    $display("All checks passed");
  end
endmodule
