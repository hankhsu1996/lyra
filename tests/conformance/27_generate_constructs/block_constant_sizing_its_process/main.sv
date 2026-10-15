// A local parameter a generate block declares is a constant (LRM 6.20.4), so
// it may bound the range of a variable the block's process declares, which
// exists only while that process runs (LRM 6.21). Written from a loop's index
// (LRM 27.4) the constant differs from one block to the next, and written from
// a parameter of the module it differs from one instance to another (LRM
// 23.10); either way each process's variable is as wide as its own block's
// constant says.
module Leaf #(parameter int P = 1);
  int width;

  if (1) begin : b
    localparam int W = P + 1;
    initial begin
      automatic logic [W:0] scratch = '1;
      width = $bits(scratch);
    end
  end
endmodule

module Top;
  localparam int N = 4;
  int widths [N];

  for (genvar i = 0; i < N; i++) begin : g
    localparam int R = i + 2;
    initial begin
      automatic logic [R:0] scratch = '1;
      widths[i] = $bits(scratch);
    end
  end

  Leaf #(.P(1)) narrow ();
  Leaf #(.P(5)) wide ();

  final begin
    for (int k = 0; k < N; k++)
      if (widths[k] !== k + 3)
        $fatal(1, "block %0d's process declares %0d bits, expected %0d", k,
               widths[k], k + 3);
    if (narrow.width !== 3)
      $fatal(1, "narrow's process declares %0d bits, expected 3",
             narrow.width);
    if (wide.width !== 7)
      $fatal(1, "wide's process declares %0d bits, expected 7", wide.width);
    $display("All checks passed");
  end
endmodule
