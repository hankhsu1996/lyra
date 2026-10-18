// @top: Top Watcher
// @reports: expect failed
//
// An expect statement blocks the process executing it until one evaluation of
// its property succeeds or fails (LRM 16.17). The evaluation starts at the next
// clocking event after the statement is reached, the arm the answer selects
// runs, and the statement after it runs after the Observed region in which the
// property completed -- so after the nonblocking assignments of that time step.
// With no else clause a failure is reported by the tool, which is the report
// this case expects.
//
// Its property reads automatic variables and a `ref` formal at their current
// values (LRM 16.5.1, 16.17), it may stand wherever a wait may, including in a
// task and in a program (LRM 24), and a process stopped while it waits ends the
// evaluation with it. Every occurrence of the clock is a tick, two in one time
// step included, and a disable condition true at any moment of the evaluation
// leaves the process waiting (LRM 16.12).
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;
  logic c = 1;
  logic d = 0;

  int ticks = 0;
  int count = 0;

  always #5 clk = ~clk;

  // Ticks land at 5, 15, 25, 35, 45 and 55. `ticks` moves before the Observed
  // region of each, `count` only in its NBA region.
  always @(posedge clk) begin
    ticks = ticks + 1;
    count <= count + 1;
  end

  initial begin
    #1;
    a = 1;
    #8;
    a = 0;
    b = 1;
    c = 0;
    #10;
    b = 0;
    d = 1;
  end

  // Success, and the statement after it running after the NBA region.
  time pass_at = -1;
  int count_seen = -1;
  int pass_runs = 0;
  int pass_fails = 0;
  initial begin
    #2;
    expect (@(posedge clk) a ##1 b) pass_runs = pass_runs + 1;
      else pass_fails = pass_fails + 1;
    pass_at = $time;
    count_seen = count;
  end

  // Failure, taken at the first tick that cannot be extended to a match.
  time fail_at = -1;
  int fail_runs = 0;
  int fail_passes = 0;
  initial begin
    #2;
    expect (@(posedge clk) c ##1 c) fail_passes = fail_passes + 1;
      else fail_runs = fail_runs + 1;
    fail_at = $time;
  end

  // With no else clause a failure is the tool's own `$error` (LRM 16.17,
  // 20.10), and the process goes on after it.
  time reported_at = -1;
  initial begin
    #2;
    expect (@(posedge clk) c ##1 c);
    reported_at = $time;
  end

  // Reached at a tick, it starts at the next one rather than that one.
  time next_at = -1;
  initial begin
    @(posedge clk);
    @(posedge clk);
    expect (@(posedge clk) d);
    next_at = $time;
  end

  // An implication whose antecedent does not match holds vacuously, which is
  // a success.
  int vacuous_runs = 0;
  initial begin
    #2;
    expect (@(posedge clk) d |-> a) vacuous_runs = vacuous_runs + 1;
      else vacuous_runs = -100;
  end

  // A task whose property reads its automatic argument (LRM 16.17's example).
  task automatic wait_for(int value, output bit success);
    expect (@(posedge clk) ##[1:10] ticks == value) success = 1;
      else success = 0;
  endtask

  // `ticks` is sampled before the tick moves it, so it reads 2 at 25.
  bit found = 0;
  time found_at = -1;
  initial begin
    wait_for(2, found);
    found_at = $time;
  end

  // Two processes inside the one automatic task at once each wait on an
  // evaluation of their own, reading their own argument.
  bit early_found = 0;
  bit late_found = 0;
  time early_at = -1;
  time late_at = -1;
  initial begin
    wait_for(1, early_found);
    early_at = $time;
  end
  initial begin
    wait_for(3, late_found);
    late_at = $time;
  end

  // A `ref` formal is read at its current value in the Observed region, which
  // the NBA region of the same time step has already written.
  task automatic wait_on(ref int r, input int value);
    expect (@(posedge clk) ##[0:5] r == value);
  endtask

  time ref_at = -1;
  initial begin
    wait_on(count, 3);
    ref_at = $time;
  end

  // A process stopped while it waits takes its evaluation with it.
  bit past_stopped = 0;
  initial begin : stopped
    expect (@(posedge clk) a ##1 b ##1 b);
    past_stopped = 1;
  end
  initial #12 disable stopped;

  // Every occurrence of the clocking event is a tick, two in one time step
  // included.
  logic g = 0;
  time glitch_at = -1;
  initial begin
    #31;
    g = 1;
    g = 0;
    g = 1;
  end
  initial begin
    expect (@(posedge g) 1'b1 ##1 1'b1);
    glitch_at = $time;
  end

  // A disable condition true at any moment of the evaluation disables it,
  // which is neither a success nor a failure, so the process does not go on
  // (LRM 16.12); here it is high only between two ticks. Where it never
  // holds the expect answers as any other does.
  logic rst = 0;
  logic quiet = 0;
  bit past_disabled = 0;
  time undisabled_at = -1;
  initial begin
    #20 rst = 1;
    #1 rst = 0;
  end
  initial begin
    #2;
    expect (@(posedge clk) disable iff (rst) 1'b1 ##3 1'b1);
    past_disabled = 1;
  end
  initial begin
    #2;
    expect (@(posedge clk) disable iff (quiet) 1'b1 ##1 1'b1);
    undisabled_at = $time;
  end

  // A loop around an expect waits again each time it is answered, starting
  // each evaluation at the tick after the one that answered the last.
  int loop_passes = 0;
  initial forever expect (@(posedge clk) 1) loop_passes = loop_passes + 1;

  // A program's expect is evaluated in the Observed region of the time step,
  // before that step's Reactive region, and continues in that Reactive region.
  time program_at = -1;
  int program_count = -1;
  time program_failed_at = -1;

  initial #60 $finish;

  final begin
    if (pass_runs !== 1 || pass_fails !== 0)
      $fatal(1, "the passing expect ran %0d pass and %0d fail arms",
             pass_runs, pass_fails);
    if (pass_at !== 15)
      $fatal(1, "the passing expect went on at %0t, expected 15", pass_at);
    if (count_seen !== 2)
      $fatal(1, "after the passing expect count was %0d, expected 2",
             count_seen);
    if (fail_runs !== 1 || fail_passes !== 0)
      $fatal(1, "the failing expect ran %0d fail and %0d pass arms",
             fail_runs, fail_passes);
    if (fail_at !== 15)
      $fatal(1, "the failing expect went on at %0t, expected 15", fail_at);
    if (reported_at !== 15)
      $fatal(1, "the expect with no else clause went on at %0t, expected 15",
             reported_at);
    if (next_at !== 25)
      $fatal(1, "the expect reached at a tick went on at %0t, expected 25",
             next_at);
    if (vacuous_runs !== 1)
      $fatal(1, "the vacuous expect ran its pass arm %0d times, expected 1",
             vacuous_runs);
    if (found !== 1 || found_at !== 25)
      $fatal(1, "the task's expect found %0d at %0t, expected 1 at 25",
             found, found_at);
    if (early_found !== 1 || early_at !== 15 || late_found !== 1 ||
        late_at !== 35)
      $fatal(1, "two calls of the task found %0d at %0t and %0d at %0t",
             early_found, early_at, late_found, late_at);
    if (ref_at !== 25)
      $fatal(1, "the expect on a ref formal went on at %0t, expected 25",
             ref_at);
    if (past_stopped !== 0)
      $fatal(1, "the stopped process went on past its expect");
    if (loop_passes !== 6)
      $fatal(1, "the looping expect held %0d times, expected 6", loop_passes);
    if (glitch_at !== 31)
      $fatal(1, "the expect on two edges at once went on at %0t, expected 31",
             glitch_at);
    if (past_disabled !== 0)
      $fatal(1, "the process went on past a disabled expect");
    if (undisabled_at !== 15)
      $fatal(1, "the expect never disabled went on at %0t, expected 15",
             undisabled_at);
    if (program_failed_at !== 5)
      $fatal(1, "the program's expect on its automatic failed at %0t",
             program_failed_at);
    if (program_at !== 25 || program_count !== 3)
      $fatal(1, "the program's expect went on at %0t seeing %0d, expected 25",
             program_at, program_count);
    $display("All checks passed");
  end
endmodule

program Watcher;
  initial begin
    #2;
    expect (@(posedge Top.clk) ##[0:5] Top.d);
    Top.program_at = $time;
    Top.program_count = Top.count;
    #100;
  end

  // Another thread of this program writes the waiting thread's automatic in
  // the Reactive region of the first tick, after the Observed region that
  // evaluates it, so the property reads it unwritten and fails there.
  initial begin
    automatic int v = 0;
    fork
      begin
        @(posedge Top.clk);
        v = 1;
      end
    join_none
    expect (@(posedge Top.clk) v == 1)
      else Top.program_failed_at = $time;
    #100;
  end
endprogram
