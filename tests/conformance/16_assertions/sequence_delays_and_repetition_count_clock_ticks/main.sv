// A sequence is a regular expression over Boolean expressions, and what it
// advances over is clock ticks rather than simulation time (LRM 16.7). `##1`
// puts the sequence that follows one tick after the current one, `##2` two, and
// `##[m:n]` admits a match anywhere in that window; a consecutive repetition
// `[*n]` requires the Boolean to hold at n successive ticks.
//
// The five assertions here read one waveform. Each is an implication whose
// antecedent matches at exactly one tick, so each has exactly one attempt that
// evaluates its consequent and the rest succeed vacuously (LRM 16.12.7). Two of
// the five are written to fail, which is what separates "the delay landed on
// the right tick" from "the delay was never counted at all".
module Top;
  logic clk = 0;
  logic req = 0;
  logic gnt = 0;
  logic busy = 0;

  int fails_two = 0;
  int fails_one = 0;
  int fails_window = 0;
  int fails_twice = 0;
  int fails_three_times = 0;

  always #5 clk = ~clk;

  // Ticks land at 5, 15, 25, 35, 45 and 55. Sampled: `req` holds only at the
  // tick at 15, `gnt` only at the tick at 35, and `busy` at the ticks at 25 and
  // 35 but not at 45.
  initial begin
    #8 req = 1;
    #10 req = 0;
    #4 busy = 1;
    #10 gnt = 1;
    #6 gnt = 0;
    #4 busy = 0;
    #16 $finish;
  end

  a_two: assert property (@(posedge clk) req |-> ##2 gnt)
    else fails_two = fails_two + 1;

  a_one: assert property (@(posedge clk) req |-> ##1 gnt)
    else fails_one = fails_one + 1;

  a_window: assert property (@(posedge clk) req |-> ##[1:2] gnt)
    else fails_window = fails_window + 1;

  a_twice: assert property (@(posedge clk) req |=> busy[*2])
    else fails_twice = fails_twice + 1;

  a_three_times: assert property (@(posedge clk) req |=> busy[*3])
    else fails_three_times = fails_three_times + 1;

  final begin
    if (fails_two !== 0)
      $fatal(1, "gnt two ticks after req was missed %0d times", fails_two);
    if (fails_one !== 1)
      $fatal(1, "gnt one tick after req failed %0d times, expected 1",
             fails_one);
    if (fails_window !== 0)
      $fatal(1, "gnt within two ticks of req was missed %0d times",
             fails_window);
    if (fails_twice !== 0)
      $fatal(1, "busy at two successive ticks was missed %0d times",
             fails_twice);
    if (fails_three_times !== 1)
      $fatal(1, "busy at three successive ticks failed %0d times, expected 1",
             fails_three_times);
    $display("All checks passed");
  end
endmodule
