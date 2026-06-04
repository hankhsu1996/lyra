// %t uses the $timeformat display unit applied design-wide (LRM 20.4.3), not the
// calling scope's own unit -- so a single $timeformat makes every scope report
// on one uniform scale, unlike $time (TS2), which scales to each scope's own
// unit. Top (1ps) sets the display unit to ns once; both scopes then print %t on
// the ns scale despite holding different time units. Top wakes at 2500 ticks
// (2500 ps = 2.5 ns); Slow wakes at #3 = 3 ns = 3000 ticks, so Top prints first.
`timescale 1ps / 1ps
module Top;
  Slow s ();
  initial begin
    $timeformat(-9, 2, "ns", 0);
    #2500 $display("Top=%t", $time);
  end
endmodule

`timescale 1ns / 1ps
module Slow;
  initial #3 $display("Slow=%t", $time);
endmodule
