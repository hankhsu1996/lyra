// A delay control suspends the procedure that reaches it and resumes it that
// much simulation time later, the amount being counted in the time unit of the
// design element the statement sits in unless the delay carries a unit of its
// own, in which case it names an amount of time that is scaled into that unit
// rather than counted in it (LRM 9.4.1, 3.14.1, 5.8). Simulation time
// therefore advances by the delay, a procedure suspended for longer resumes
// later, and a value written at an earlier time is in place by the time a
// procedure delayed past it resumes.
module Top;
  timeunit 1ns;
  timeprecision 1ps;

  time at_start;
  time after_units;
  time after_named_unit;
  time after_smaller_unit;
  time long_wake;
  int handoff;
  int seen_before_handoff;
  int seen_after_handoff;

  initial begin
    at_start = $time;
    #5;
    after_units = $time;
    #5ns;
    after_named_unit = $time;
    #5000ps;
    after_smaller_unit = $time;
  end

  initial begin
    #5;
    handoff = 1;
  end

  initial begin
    #2;
    seen_before_handoff = handoff;
  end

  initial begin
    #10;
    long_wake = $time;
    seen_after_handoff = handoff;
  end

  final begin
    if (at_start !== 0) $fatal(1, "at_start was %0d, expected 0", at_start);
    if (after_units !== 5)
      $fatal(1, "after_units was %0d, expected 5", after_units);
    if (after_named_unit !== 10)
      $fatal(1, "after_named_unit was %0d, expected 10", after_named_unit);
    if (after_smaller_unit !== 15)
      $fatal(1, "after_smaller_unit was %0d, expected 15", after_smaller_unit);
    if (long_wake !== 10)
      $fatal(1, "long_wake was %0d, expected 10", long_wake);
    if (seen_before_handoff !== 0)
      $fatal(1, "seen_before_handoff was %0d, expected 0",
             seen_before_handoff);
    if (seen_after_handoff !== 1)
      $fatal(1, "seen_after_handoff was %0d, expected 1", seen_after_handoff);
    $display("All checks passed");
  end
endmodule
