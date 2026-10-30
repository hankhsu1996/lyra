// A declaration whose range a query on an indexed part-select states, where a
// loop index or a parameter places the select and sets its width. The query
// answers from the select's type (LRM 20.7), so each block and each instance
// declares the range its own select gives, and its width follows (LRM 7.4.1).
module Sized #(parameter int B = 0) (input logic [31:0] d, output int bits);
  logic [$bits(d[B*8+:B+1])-1:0] x;
  initial bits = $bits(x);
endmodule

module Top;
  logic [31:0] d;
  int got[4];
  int bits0, bits1;

  for (genvar b = 0; b < 4; b++) begin : g
    logic [$bits(d[b*8+:b+1])-1:0] x;
    initial got[b] = $bits(x);
  end

  Sized #(.B(0)) s0 (.d(d), .bits(bits0));
  Sized #(.B(1)) s1 (.d(d), .bits(bits1));

  initial begin
    #1;
    for (int b = 0; b < 4; b++)
      if (got[b] !== b + 1)
        $fatal(1, "block %0d measured %0d, expected %0d", b, got[b], b + 1);
    if (got[0] !== $bits(g[0].x) || got[3] !== $bits(g[3].x))
      $fatal(1, "blocks measured %0d and %0d", got[0], got[3]);
    if (bits0 !== 1 || bits1 !== 2)
      $fatal(1, "instances measured %0d and %0d, expected 1 and 2", bits0,
             bits1);
    $display("All checks passed");
  end
endmodule
