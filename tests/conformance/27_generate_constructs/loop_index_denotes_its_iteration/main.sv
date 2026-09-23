// Within the generate block of a loop generate construct the loop index name
// denotes an implicit localparam whose value in each instance is the value the
// index held when that instance was elaborated (LRM 27.4). That one rule
// answers every position the name can appear in, and the positions compose
// differently: as an ordinary value, inside an expression, as the bound of a
// declared range so that each instance's type differs, as the key an assignment
// pattern designates an element by (LRM 10.9.1), and as part of the
// hierarchical name a reference from outside the construct spells (LRM 23.6).
// The clause names that last position twice over: a declaration of the block is
// reached that way, and so is the implicit localparam itself, which LRM 27.4
// says "can be referenced with a hierarchical name".
module Top;
  localparam int N = 8;

  int seen [N];
  int scaled [N];
  int widths [N];
  int keyed [N];

  for (genvar i = 0; i < N; i++) begin : g
    logic [i:0] slice;
    int marked [N];
    initial begin
      seen[i] = i;
      scaled[i] = (i * 3) - 1;
      slice = '1;
      widths[i] = $bits(slice);
      marked = '{default: 0, i: 9};
      keyed[i] = marked[i];
    end
  end

  int reached;
  int reached_index;
  initial begin
    reached = $bits(g[5].slice);
    reached_index = g[5].i;
  end

  final begin
    for (int k = 0; k < N; k++) begin
      if (seen[k] !== k)
        $fatal(1, "iteration %0d denotes index %0d", k, seen[k]);
      if (scaled[k] !== (k * 3) - 1)
        $fatal(1, "iteration %0d scaled to %0d, expected %0d", k, scaled[k],
               (k * 3) - 1);
      if (widths[k] !== k + 1)
        $fatal(1, "iteration %0d declares %0d bits, expected %0d", k, widths[k],
               k + 1);
      if (keyed[k] !== 9)
        $fatal(1, "iteration %0d designated element %0d, which holds %0d", k, k,
               keyed[k]);
    end
    if (reached !== 6)
      $fatal(1, "g[5].slice is %0d bits, expected 6", reached);
    if (reached_index !== 5)
      $fatal(1, "g[5].i is %0d, expected 5", reached_index);
    $display("All checks passed");
  end
endmodule
