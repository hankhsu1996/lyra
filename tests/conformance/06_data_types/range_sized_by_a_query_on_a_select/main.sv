// A declaration whose range a query on an indexed part-select states, where a
// loop index or a parameter places the select. The query answers from the
// select's type (LRM 20.7), so each block and each instance declares the range
// its own select gives, and its width follows (LRM 7.4.1).
module Sized #(parameter int B = 0) (input logic [31:0] d, output int bits);
  logic [$left(d[B*8+:8]):0] x;
  initial bits = $bits(x);
endmodule

module Top;
  logic [31:0] d;
  int got[4];
  int bits0, bits1;

  for (genvar b = 0; b < 4; b++) begin : g
    logic [$left(d[b*8+:8]):0] x;
    initial got[b] = $bits(x);
  end

  Sized #(.B(0)) s0 (.d(d), .bits(bits0));
  Sized #(.B(1)) s1 (.d(d), .bits(bits1));

  initial begin
    #1;
    if (got[0] !== $bits(g[0].x) || got[3] !== $bits(g[3].x))
      $fatal(1, "blocks measured %0d and %0d", got[0], got[3]);
    if (got[0] == got[3]) $fatal(1, "blocks of different positions measured alike, %0d", got[0]);
    if (bits0 == bits1) $fatal(1, "instances of different positions measured alike, %0d", bits0);
    if (bits0 !== got[0] || bits1 !== got[1])
      $fatal(1, "instances measured %0d and %0d, blocks %0d and %0d", bits0, bits1, got[0],
             got[1]);
    $display("All checks passed");
  end
endmodule
