// Turning a concurrent assertion off stops new attempts and leaves the ones
// already executing, and their pass or fail statements, as they are; killing
// it aborts those too, and turning it on again afterwards does not bring them
// back (LRM 20.11), whether the call names every assertion or this one by its
// label. The action controls decide what an attempt's outcome runs
// as they stood when the attempt began, so a change made while it is in
// flight does not reach it.
//
// `a |=> b` holds vacuously at every tick where `a` is low (LRM 16.14.8), and
// `$assertvacuousoff` keeps those successes from running the pass statement,
// so the counts below are of attempts whose antecedent matched. Ticks land at
// 10k + 5, and every value is written at a multiple of 10, between ticks.
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;
  int passes = 0;
  int fails = 0;

  always #5 clk = ~clk;

  p : assert property (@(posedge clk) a |=> b)
    passes = passes + 1;
  else
    fails = fails + 1;

  task automatic At(input int t);
    #(t - $time);
  endtask

  task automatic Expect(input int want_passes, input string step);
    if (passes !== want_passes || fails !== 0) begin
      $fatal(1, "%s: %0d passes and %0d fails, expected %0d and 0", step,
             passes, fails, want_passes);
    end
  endtask

  initial begin
    $assertvacuousoff;

    // An attempt begins at 15; checking is turned off before it reads `b` at
    // 25, which it still does, running its pass statement.
    At(10);
    a = 1;
    At(20);
    $assertoff;
    a = 0;
    b = 1;
    At(40);
    Expect(1, "off leaves an attempt in flight");

    // With checking off, the antecedent at 45 begins nothing.
    b = 0;
    a = 1;
    At(50);
    a = 0;
    At(60);
    Expect(1, "off begins no attempt");

    // An attempt begins at 65 that would fail at 75; it is killed at 70, and
    // checking is on again before 75.
    $asserton;
    a = 1;
    At(70);
    a = 0;
    $assertkill;
    $asserton;
    At(90);
    Expect(1, "kill aborts an attempt in flight");

    // An attempt begins at 95 while passing is on; passing is turned off
    // before it succeeds at 105, and its statement still runs.
    a = 1;
    At(100);
    a = 0;
    $assertpassoff;
    b = 1;
    At(120);
    Expect(2, "pass off leaves an attempt that began before it");

    // One beginning at 125 succeeds at 135 without running it.
    b = 0;
    a = 1;
    At(130);
    a = 0;
    b = 1;
    At(140);
    Expect(2, "pass off");

    // One beginning at 145 fails at 155, and with failing off runs nothing.
    $assertfailoff;
    b = 0;
    a = 1;
    At(150);
    a = 0;
    At(170);
    Expect(2, "fail off");

    // Nonvacuous successes run the pass statement again, and vacuous ones
    // still do not: the one beginning at 175 succeeds at 185.
    $assertnonvacuouson;
    b = 1;
    a = 1;
    At(180);
    a = 0;
    At(200);
    Expect(3, "nonvacuous on");

    // The statement's label names it (LRM 16.3), so turning `p` off by name
    // begins no attempt at 205 either.
    $assertoff(0, p);
    a = 1;
    At(210);
    a = 0;
    At(230);
    Expect(3, "off by its label");
    $display("All checks passed");
    $finish;
  end
endmodule
