// A compilation-unit scope that states no time unit is governed by the
// default time unit and precision, which the standard leaves to the tool
// (LRM 3.14.2.3) and Lyra makes 1ns, and takes no part in the global time
// precision (3.14.3). Where every design element states a coarser one, the
// compilation unit's time unit is finer than the tick. Time read there is in
// its own unit (20.3.1), so two microseconds read as 2000; a delay there
// names less than a tick, and what that becomes the standard does not say:
// Lyra rounds it to the nearest tick, so one nanosecond is no wait at all.
`timescale 1us / 1us

function automatic time unit_now();
  return $time;
endfunction

task automatic unit_wait();
  #1;
endtask

module Top;
  time read_there;
  time after_wait;

  initial begin
    #2;
    read_there = unit_now();
    unit_wait();
    after_wait = $time;
  end

  final begin
    if (read_there !== 2000)
      $fatal(1, "$time in the compilation unit read %0d, expected 2000",
             read_there);
    if (after_wait !== 2)
      $fatal(1, "a nanosecond's delay left the time at %0d, expected 2",
             after_wait);
    $display("All checks passed");
  end
endmodule
