// A disable target declared by another scope of the same module is reached by
// a hierarchical name like any other (LRM 9.6.2, 23.6), and the reach stays
// inside the module's own layout: a generate block is a scope this module lays
// out, so the route to one is typed and the target is that block's own. The
// same holds between two loop-generate iterations, where the label alone does
// not say which iteration is meant and the index does. A target the disabling
// body's own scope declares is the empty case of that reach and needs no name
// beyond its own.
module Top;
  int own_reached = 0;
  int own_finished = 0;
  int gen_reached = 0;
  int gen_finished = 0;
  int lane_finished[2];
  int sibling_seen = 0;

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

  // One iteration ends another's block by naming it with an index, which is
  // the reach between two scopes the same module lays out.
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
    $display("All checks passed");
  end
endmodule
