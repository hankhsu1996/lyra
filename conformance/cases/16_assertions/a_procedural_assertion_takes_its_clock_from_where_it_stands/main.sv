// A procedural concurrent assertion that writes no clock takes the one the
// procedure around it settles -- the edge of its one event control, where
// nothing else in the body reads that edge's signal -- and where the procedure
// settles none, the default clocking (LRM 16.14.6, 14.12). Whatever the kind
// of procedure, the attempts are the instances it queued.
module Top;
  logic clk = 0;
  logic a = 0;

  int ff_hits = 0;
  int init_hits = 0;
  int comb_hits = 0;
  int neg_hits = 0;

  default clocking @(posedge clk); endclocking

  // Rising edges at 5, 15, 25 and 35; falling at 10, 20 and 30.
  always #5 clk = ~clk;

  // The clock is the `always_ff`'s own edge, and each run queues one instance.
  always_ff @(posedge clk) begin
    c_ff: cover property (1) ff_hits++;
  end

  // An `initial` past its event control queues once, at the tick at 5.
  initial begin
    @(posedge clk);
    c_init: cover property (1) init_hits++;
  end

  // An `always_comb` settles no clock, so the default clocking counts the
  // ticks. It runs once at time 0, and the instance it queued begins at 5.
  always_comb begin
    c_comb: cover property (a == 1'b0) comb_hits++;
  end

  // The falling edge is the procedure's clock.
  always @(negedge clk) begin
    c_neg: cover property (1) neg_hits++;
  end

  initial #36 $finish;

  final begin
    if (ff_hits !== 4)
      $fatal(1, "the always_ff covered %0d, expected 4", ff_hits);
    if (init_hits !== 1)
      $fatal(1, "the initial covered %0d, expected 1", init_hits);
    if (comb_hits !== 1)
      $fatal(1, "the always_comb covered %0d, expected 1", comb_hits);
    if (neg_hits !== 3)
      $fatal(1, "the falling-edge procedure covered %0d, expected 3", neg_hits);
    $display("All checks passed");
  end
endmodule
