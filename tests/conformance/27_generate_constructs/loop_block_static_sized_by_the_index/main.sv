// A loop generate's block holds a procedure whose named block declares a
// static variable as wide as the block's index says (LRM 27.4). A static
// variable exists for the whole run and is reached by a hierarchical name
// through the named block (LRM 6.21, 23.6), so each block's variable has its
// own width, seen from inside the procedure and from outside the construct.
module Top;
  localparam int N = 4;
  int from_block [N];
  int outside;

  for (genvar i = 0; i < N; i++) begin : g
    initial begin : p
      static logic [i:0] s = '1;
      from_block[i] = $bits(s);
    end
  end

  initial outside = $bits(g[2].p.s);

  final begin
    for (int k = 0; k < N; k++)
      if (from_block[k] !== k + 1)
        $fatal(1, "block %0d's static is %0d bits, expected %0d", k,
               from_block[k], k + 1);
    if (outside !== 3)
      $fatal(1, "g[2].p.s is %0d bits from outside, expected 3", outside);
    $display("All checks passed");
  end
endmodule
