// An array query on an expression answers from the expression's type (LRM
// 20.7), and the standard does not say what index range the type of a
// part-select has: the indices it selects in the vector, or a range of the
// same width numbered from 0. Lyra keeps the indices it selects, so the left
// bound of an indexed part-select is the index its position and width name,
// in each block of a loop and each instance of a parameter. This case holds
// Lyra to that.
module Sized #(parameter int B = 0) (input logic [31:0] d, output int left);
  initial left = $left(d[B*8+:8]);
endmodule

module Top;
  logic [31:0] d;
  int got[4];
  int left0, left1;

  for (genvar b = 0; b < 4; b++) begin : g
    logic [$left(d[b*8+:8]):0] x;
    initial got[b] = $bits(x);
  end

  Sized #(.B(0)) s0 (.d(d), .left(left0));
  Sized #(.B(1)) s1 (.d(d), .left(left1));

  initial begin
    #1;
    for (int b = 0; b < 4; b++)
      if (got[b] !== b * 8 + 8)
        $fatal(1, "block %0d measured %0d, expected %0d", b, got[b], b * 8 + 8);
    if ($left(d[19:12]) !== 19 || $right(d[19:12]) !== 12)
      $fatal(1, "a part-select of 19 to 12 had bounds %0d and %0d",
             $left(d[19:12]), $right(d[19:12]));
    if (left0 !== 7 || left1 !== 15)
      $fatal(1, "instances answered %0d and %0d, expected 7 and 15", left0,
             left1);
    $display("All checks passed");
  end
endmodule
