// $time reports time in the calling scope's own unit (LRM 20.3), while every
// scope shares one design-global tick (LRM 3.14.3). The global precision here
// is 1ps. Top (unit 1ps) wakes at 2500 ticks and reads 2500; the child Slow
// (unit 1ns) wakes at #3 = 3 ns = 3000 ticks and reads 3. Top's read prints
// first because 2500 ticks precedes 3000 ticks on the shared axis, proving both
// the shared time axis and the per-scope unit scaling.
`timescale 1ps / 1ps
module Top;
  Slow s ();
  initial #2500 $display("Top=%0d", $time);
endmodule

`timescale 1ns / 1ps
module Slow;
  initial #3 $display("Slow=%0d", $time);
endmodule
