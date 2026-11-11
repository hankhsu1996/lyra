// Asked of no object, the time unit and the time precision are the simulation
// time unit (LRM 38.6): the smallest precision the design states anywhere,
// a package's as much as a module's (3.14.3), so a package finer than every
// module sets it. DPI's own queries of no scope answer the same two values
// (H.13).
package Fine;
  timeunit 1ps;
  timeprecision 100fs;
endpackage

`timescale 1us / 1us
module Leaf;
endmodule

module Top;
  import "DPI-C" context function int inspect();

  Leaf leaf ();

  int differences = -1;

  initial differences = inspect();

  final begin
    if (differences !== 0)
      $fatal(1, "the inspection found %0d differences", differences);
    $display("All checks passed");
  end
endmodule
