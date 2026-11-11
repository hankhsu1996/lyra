// Where a sampled value function asks for a past value of a constant cast, the
// cast's current value is taken instead (LRM 16.5.1): a constant cast has no
// history, so `$past(const'(a))` is what `a` is now, whatever it was at the
// tick before, and a value change function finds a constant cast unchanged.
// This holds in an assertion, in procedural code and in a continuous
// assignment alike. Around a cast, the rest of the operand is read as of the
// tick named, so `$past(arr[const'(j)])` is the earlier `arr` at the `j` of
// now, and a function a cast calls is called where the cast is read.
module Top;
  logic clk = 0;
  logic [3:0] a = 4'd1;
  logic [3:0] arr[4] = '{4'd1, 4'd2, 4'd3, 4'd4};
  int j = 0;
  int ticks = 0;

  int fails = 0;
  int stable_fails = 0;
  int changed_fails = 0;
  int index_fails = 0;
  int call_fails = 0;
  int passes = 0;

  default clocking @(posedge clk); endclocking

  always #5 clk = ~clk;

  // `a` steps at every falling edge, so at each rising edge after the first
  // its value differs from the one it had a tick earlier.
  always @(negedge clk) a = a + 4'd1;

  now: assert property (@(posedge clk) $past(const'(a)) == const'(a))
    passes++; else fails++;
  same: assert property (@(posedge clk) $stable(const'(a)))
    passes++; else stable_fails++;
  unchanged: assert property (@(posedge clk) !$changed(const'(a)))
    passes++; else changed_fails++;

  // A continuous assignment reads the cast as it is now too.
  logic [3:0] mirrored;
  assign mirrored = $past(const'(a));

  function automatic logic [3:0] doubled(logic [3:0] v);
    return v * 2;
  endfunction

  logic [3:0] arr_before[4];
  always @(posedge clk) begin
    ticks++;
    // `arr` and `j` change here, after this tick has read them.
    if ($past(arr[const'(j)]) !== (ticks == 1 ? 4'd1 + j : arr_before[j]))
      index_fails++;
    if (!$stable(const'(doubled(a)))) call_fails++;
    arr_before = arr;
    j = (j + 1) % 4;
    foreach (arr[k]) arr[k] = arr[k] + 4'd4;
  end

  // `a` settled at the falling edge before, so the assignment has too.
  always @(posedge clk)
    if (mirrored !== a) fails++;

  initial #40 $finish;

  final begin
    if (fails !== 0)
      $fatal(1, "a past value of the cast differed from it %0d times", fails);
    if (stable_fails !== 0 || changed_fails !== 0)
      $fatal(1, "the cast was found changed %0d and %0d times", stable_fails,
             changed_fails);
    if (index_fails !== 0)
      $fatal(1, "an index cast around a past array was wrong %0d times",
             index_fails);
    if (call_fails !== 0)
      $fatal(1, "a cast of a call was found changed %0d times", call_fails);
    // The clock rises at 5, 15, 25 and 35, before the run ends at 40.
    if (ticks != 4) $fatal(1, "the clock ticked %0d times, expected 4", ticks);
    if (passes != 3 * 4)
      $fatal(1, "the assertions passed %0d times, expected %0d", passes, 3 * 4);
    $display("All checks passed");
  end
endmodule
