// The three simulation time functions all report the current simulation time
// scaled to the time unit of the design element that invoked them, and differ
// only in the answer they give it in: $time is a 64-bit integer and rounds to
// it, $stime is the low 32 bits of that same integer, and $realtime is a real
// and keeps whatever fraction of a time unit the instant falls on
// (LRM 20.3.1, 20.3.2, 20.3.3). Two elements holding different time units
// therefore report different numbers for one and the same instant.
`timescale 1ps / 1ps
module Top;
  Slow slow ();

  time fine_time;
  int fine_stime;
  real fine_realtime;

  initial begin
    #3000;
    fine_time = $time;
    fine_stime = $stime;
    fine_realtime = $realtime;
  end

  final begin
    if (fine_time !== 3000)
      $fatal(1, "fine_time was %0d, expected 3000", fine_time);
    if (fine_stime !== 3000)
      $fatal(1, "fine_stime was %0d, expected 3000", fine_stime);
    if (fine_realtime != 3000.0)
      $fatal(1, "fine_realtime was %g, expected 3000", fine_realtime);

    if (slow.coarse_time !== 3)
      $fatal(1, "slow.coarse_time was %0d, expected 3", slow.coarse_time);
    if (slow.coarse_stime !== 3)
      $fatal(1, "slow.coarse_stime was %0d, expected 3", slow.coarse_stime);
    if (slow.coarse_realtime != 3.0)
      $fatal(1, "slow.coarse_realtime was %g, expected 3",
             slow.coarse_realtime);

    if (slow.fraction_time !== 5)
      $fatal(1, "slow.fraction_time was %0d, expected 5",
             slow.fraction_time);
    if (slow.fraction_realtime != 5.25)
      $fatal(1, "slow.fraction_realtime was %g, expected 5.25",
             slow.fraction_realtime);
    $display("All checks passed");
  end
endmodule

`timescale 1ns / 1ps
module Slow;
  time coarse_time;
  int coarse_stime;
  real coarse_realtime;
  time fraction_time;
  real fraction_realtime;

  initial begin
    #3;
    coarse_time = $time;
    coarse_stime = $stime;
    coarse_realtime = $realtime;
    #2.25;
    fraction_time = $time;
    fraction_realtime = $realtime;
  end
endmodule
