// A loop generate counts out a block for each value its header steps through
// (LRM 27.4), so how far it counts decides which blocks an instance builds
// wherever its blocks are not all alike: a chain whose first block differs
// from the rest, or blocks whose declarations follow the index. Here the count
// is a parameter one module hands another (LRM 23.10), and each instance of
// the middle module is handed a different one, so each holds the blocks its
// own count says.
module Chain #(
    parameter int N = 1
) (
    output int first,
    output int rest,
    output int widest
);
  int seen_rest;
  int width;
  for (genvar i = 0; i < N; i++) begin : link
    if (i == 0) begin : seed
      assign first = 1;
    end else begin : step
      initial seen_rest = seen_rest + 1;
    end
  end
  for (genvar i = 0; i < N; i++) begin : sized
    logic [i:0] x;
    initial if ($bits(x) > width) width = $bits(x);
  end
  assign rest = seen_rest;
  assign widest = width;
endmodule

module Holder #(
    parameter int P = 1
) (
    output int first,
    output int rest,
    output int widest
);
  Chain #(.N(P)) chain (.first(first), .rest(rest), .widest(widest));
endmodule

module Top;
  int first1, rest1, widest1;
  int first3, rest3, widest3;

  Holder #(.P(1)) one (.first(first1), .rest(rest1), .widest(widest1));
  Holder #(.P(3)) three (.first(first3), .rest(rest3), .widest(widest3));

  initial begin
    #1;
    if (first1 !== 1 || first3 !== 1)
      $fatal(1, "a seed block is missing: %0d %0d", first1, first3);
    if (rest1 !== 0) $fatal(1, "P=1 built %0d step blocks", rest1);
    if (rest3 !== 2) $fatal(1, "P=3 built %0d step blocks, expected 2", rest3);
    if (widest1 !== 1) $fatal(1, "P=1 widest block is %0d bits", widest1);
    if (widest3 !== 3)
      $fatal(1, "P=3 widest block is %0d bits, expected 3", widest3);
    $display("All checks passed");
    $finish;
  end
endmodule
