// A local parameter may be declared in a generate block (LRM 6.20.4), and
// inside a loop generate's block the index name denotes an implicit localparam
// holding the value the index had when that block elaborated (LRM 27.4). So a
// constant declared there and written from the index takes a different value in
// every block, and the positions such a constant appears in compose
// differently: as an ordinary operand, as the right side of a second constant
// that reads the first, as a case item the standard leaves an ordinary
// expression (LRM 12.5), as a bit index, as the bound of a declared range so
// that each block's type differs, and as part of the hierarchical name a
// reference from outside the construct spells (LRM 27.4, 23.6).
module Top;
  localparam int N = 8;

  int seen [N];
  int chained [N];
  int matched [N];
  int picked [N];
  int widths [N];

  for (genvar i = 0; i < N; i++) begin : g
    localparam int K = (i * 3) + 1;
    localparam int M = K * 2;
    localparam int B = i % 4;
    logic [K:0] slice;
    logic [31:0] bits;

    initial begin
      seen[i] = K;
      chained[i] = M;
      slice = '1;
      widths[i] = $bits(slice);

      case (K)
        1: matched[i] = 100;
        4: matched[i] = 200;
        default: matched[i] = K;
      endcase

      bits = 32'h0;
      bits[B] = 1'b1;
      picked[i] = int'(bits);
    end
  end

  int reached;
  int reached_width;
  initial begin
    reached = g[5].K;
    reached_width = $bits(g[5].slice);
  end

  final begin
    for (int k = 0; k < N; k++) begin
      if (seen[k] !== (k * 3) + 1)
        $fatal(1, "block %0d settled %0d, expected %0d", k, seen[k],
               (k * 3) + 1);
      if (chained[k] !== ((k * 3) + 1) * 2)
        $fatal(1, "block %0d chained to %0d, expected %0d", k, chained[k],
               ((k * 3) + 1) * 2);
      if (widths[k] !== (k * 3) + 2)
        $fatal(1, "block %0d declares %0d bits, expected %0d", k, widths[k],
               (k * 3) + 2);
      if (picked[k] !== (1 << (k % 4)))
        $fatal(1, "block %0d set bit pattern %0d, expected %0d", k, picked[k],
               1 << (k % 4));
    end
    if (matched[0] !== 100)
      $fatal(1, "block 0 matched %0d, expected the item for 1", matched[0]);
    if (matched[1] !== 200)
      $fatal(1, "block 1 matched %0d, expected the item for 4", matched[1]);
    for (int k = 2; k < N; k++) begin
      if (matched[k] !== (k * 3) + 1)
        $fatal(1, "block %0d matched %0d, expected the default", k, matched[k]);
    end
    if (reached !== 16)
      $fatal(1, "g[5].K is %0d, expected 16", reached);
    if (reached_width !== 17)
      $fatal(1, "g[5].slice is %0d bits, expected 17", reached_width);
    $display("All checks passed");
  end
endmodule
