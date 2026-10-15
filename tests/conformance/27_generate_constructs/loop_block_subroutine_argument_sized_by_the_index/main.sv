// A loop generate's block declares a function whose argument is as wide as the
// block's index says (LRM 27.4, 13.4). The argument is part of how the
// function is called, so each block's function takes an argument of its own
// width, called from the block and by a hierarchical name from outside it.
module Top;
  localparam int N = 4;
  int from_block [N];
  int outside;

  for (genvar i = 0; i < N; i++) begin : g
    function automatic int width_of(input logic [i:0] a);
      logic [i:0] copy;
      copy = a;
      return $bits(copy) + $bits(a);
    endfunction
    initial from_block[i] = width_of('1);
  end

  initial outside = g[3].width_of('1);

  final begin
    for (int k = 0; k < N; k++)
      if (from_block[k] !== 2 * (k + 1))
        $fatal(1, "block %0d's function answered %0d, expected %0d", k,
               from_block[k], 2 * (k + 1));
    if (outside !== 8)
      $fatal(1, "g[3].width_of answered %0d, expected 8", outside);
    $display("All checks passed");
  end
endmodule
