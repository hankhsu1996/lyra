// Attempts overlap, and each one carries its own result. A new attempt begins
// at every tick (LRM 16.14.5), so attempts that began at different ticks can be
// in flight together and can reach their answers in the same time step -- and
// when they do, each runs the action block on its own behalf (LRM 16.14.1).
//
// The antecedent here admits a match that starts either at the tick where
// `trig` is sampled high or at the tick before it, so two attempts reach the
// same end point and evaluate the consequent at the same tick. `ack` is low
// there, so both are false and the fail statements run twice for one time step.
// A tool that carried one evaluation for the whole assertion rather than one
// per attempt answers this with a single failure, which is what the pass count
// beside it pins down.
module Top;
  logic clk = 0;
  logic trig = 0;
  logic ack = 0;

  int passes = 0;
  int fails = 0;

  always #5 clk = ~clk;

  // Ticks land at 5, 15, 25, 35 and 45. `trig` is sampled high only at the tick
  // at 25, and `ack` only at the tick at 45.
  initial begin
    #18 trig = 1;
    #10 trig = 0;
    #10 ack = 1;
    #10 $finish;
  end

  a_two_starts: assert property (@(posedge clk) ##[0:1] trig |-> ack)
      passes = passes + 1;
    else
      fails = fails + 1;

  final begin
    // The attempts that began at 15 and at 25 both match the antecedent at the
    // tick at 25; the other three never match it and are true without reading
    // `ack`.
    if (fails !== 2)
      $fatal(1, "%0d attempts failed at the same tick, expected 2", fails);
    if (passes !== 3)
      $fatal(1, "%0d attempts held, expected 3", passes);
    $display("All checks passed");
  end
endmodule
