// A disable target declared by another scope of the same module is reached by
// a hierarchical name like any other (LRM 9.6.2, 23.6), a generate block's
// included. The same holds of a block of a loop generate, where the label
// alone does not say which block is meant and the select does, whether a
// sibling block writes it, a block of the same loop computes it from its own
// index, or every block of a loop names the first one by a constant. A
// target the disabling body's own scope declares is the empty case of that
// reach and needs no name beyond its own.
module Top;
  int own_reached = 0;
  int own_finished = 0;
  int gen_reached = 0;
  int gen_finished = 0;
  int lane_finished[2];
  int sibling_seen = 0;
  int chain_finished[3];

  if (1) begin : g
    initial begin : work
      #1;
      gen_reached = 1;
      #10;
      gen_finished = 1;
    end
  end

  for (genvar i = 0; i < 2; i = i + 1) begin : lane
    initial begin : work
      #1;
      #10;
      lane_finished[i] = 1;
    end
  end

  for (genvar j = 0; j < 3; j++) begin : chain
    initial begin : work
      #1;
      #10;
      chain_finished[j] = 1;
    end
    if (j > 0) begin : stops_below
      initial begin
        #2;
        disable chain[j - 1].work;
      end
    end
  end

  // Every block of this loop ends its first block's work, its own included.
  for (genvar f = 0; f < 3; f++) begin : first
    int done = 0;
    initial begin : work
      #1;
      #10;
      done = 1;
    end
    initial begin
      #2;
      disable first[0].work;
    end
  end

  // A sibling block ends a block of the loop by naming it with a select.
  if (1) begin : stopper
    initial begin
      #2;
      sibling_seen = 1;
      disable lane[1].work;
    end
  end

  initial begin : own
    #1;
    own_reached = 1;
    if (own_reached == 1) disable own;
    own_finished = 1;
  end

  initial begin
    #2;
    disable g.work;
  end

  final begin
    if (own_reached !== 1)
      $fatal(1, "own_reached was %0d, expected 1", own_reached);
    if (own_finished !== 0)
      $fatal(1, "own_finished was %0d, expected 0", own_finished);
    if (gen_reached !== 1)
      $fatal(1, "gen_reached was %0d, expected 1", gen_reached);
    if (gen_finished !== 0)
      $fatal(1, "gen_finished was %0d, expected 0", gen_finished);
    if (sibling_seen !== 1)
      $fatal(1, "sibling_seen was %0d, expected 1", sibling_seen);
    if (lane_finished[0] !== 1)
      $fatal(1, "lane_finished[0] was %0d, expected 1", lane_finished[0]);
    if (lane_finished[1] !== 0)
      $fatal(1, "lane_finished[1] was %0d, expected 0", lane_finished[1]);
    if (chain_finished[0] !== 0 || chain_finished[1] !== 0 ||
        chain_finished[2] !== 1)
      $fatal(1, "chain finished %0d %0d %0d, expected 0 0 1", chain_finished[0],
             chain_finished[1], chain_finished[2]);
    if (first[0].done !== 0 || first[1].done !== 1 || first[2].done !== 1)
      $fatal(1, "first finished %0d %0d %0d, expected 0 1 1", first[0].done,
             first[1].done, first[2].done);
    $display("All checks passed");
  end
endmodule
