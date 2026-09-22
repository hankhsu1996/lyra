// @reports: declared bound
//
// A loop generate's index names an implicit localparam whose value is the one
// the index held when that block was elaborated (LRM 27.4), and a queue's
// declared bound is a position that value may be written in. So each generated
// block holds a queue bounded by its own index, and a write that would leave
// one longer than its own bound discards what lies past it (LRM 7.10.5).
module Top;
  localparam int N = 4;
  localparam int PUSHED = N + 2;

  int held [N + 1];
  int lowest [N + 1];
  int highest [N + 1];

  for (genvar i = 1; i <= N; i++) begin : g
    int q [$:i];
    initial begin
      for (int k = 0; k < PUSHED; k++) q.push_back(k);
      held[i] = q.size();
      lowest[i] = q[0];
      highest[i] = q[q.size() - 1];
    end
  end

  final begin
    for (int k = 1; k <= N; k++) begin
      if (held[k] !== k + 1)
        $fatal(1, "block %0d holds %0d elements, expected %0d", k, held[k],
               k + 1);
      if (lowest[k] !== 0)
        $fatal(1, "block %0d begins with %0d, expected 0", k, lowest[k]);
      if (highest[k] !== k)
        $fatal(1, "block %0d ends with %0d, expected %0d", k, highest[k], k);
    end
    $display("All checks passed");
  end
endmodule
