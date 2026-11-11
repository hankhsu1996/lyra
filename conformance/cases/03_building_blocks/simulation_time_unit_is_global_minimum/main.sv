// Every design element measures its delays in its own time unit and is
// accurate to its own time precision, but the whole design shares one time
// axis whose step is the global time precision, the smallest precision any
// design element in it asks for, a package as much as a module (LRM 3.14.1,
// 3.14.3). A delay written in one element is therefore placed on that shared
// axis in terms of real elapsed time: two elements whose time units differ
// order their delays against each other by the time each one names and not by
// the bare numbers, and an element asking for a finer precision than another
// still resolves a delay the coarser one could not express, as a package finer
// than every module does here. $time
// reads that shared time back scaled to the time unit of the element that asks
// for it, and $realtime keeps the fraction (LRM 20.3.1, 20.3.3).
package Fine;
  timeunit 1ps;
  timeprecision 100fs;

  task automatic wait_tenth();
    #0.1;
  endtask
endpackage

`timescale 1ns / 1ps
module Top;
  Coarse coarse ();

  time woke_at;
  int coarse_seen_early;
  time woke_late_at;
  realtime fine_at;

  initial begin
    Fine::wait_tenth();
    fine_at = $realtime;
  end

  initial begin
    #500ps;
    #500ps;
    woke_at = $time;
    coarse_seen_early = coarse.woke;
    #2000;
    woke_late_at = $time;
  end

  final begin
    if (woke_at !== 1) $fatal(1, "woke_at was %0d, expected 1", woke_at);
    if (coarse_seen_early !== 0)
      $fatal(1, "coarse_seen_early was %0d, expected 0", coarse_seen_early);
    if (woke_late_at !== 2001)
      $fatal(1, "woke_late_at was %0d, expected 2001", woke_late_at);
    if (coarse.woke !== 1)
      $fatal(1, "coarse.woke was %0d, expected 1", coarse.woke);
    if (coarse.woke_at !== 1)
      $fatal(1, "coarse.woke_at was %0d, expected 1", coarse.woke_at);
    if (fine_at < 0.00005 || fine_at > 0.00015)
      $fatal(1, "fine_at was %f, expected 0.0001", fine_at);
    $display("All checks passed");
  end
endmodule

`timescale 1us / 1ns
module Coarse;
  int woke;
  time woke_at;

  initial begin
    #1;
    woke = 1;
    woke_at = $time;
  end
endmodule
