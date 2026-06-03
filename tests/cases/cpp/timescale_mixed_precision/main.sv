// A design mixing time precisions shares one time axis whose tick is the
// global (finest) precision (LRM 3.14.3). Top is 1ns/1ps; the child Coarse is
// 1ns/1ns. The global precision is 1ps, so Top's `#1` lands at 1000 ticks (1 ns)
// and Coarse's `#5` at 5000 ticks (5 ns): "a" prints before "b".
//
// If Coarse's delay were scaled to its own 1ns precision instead of the global
// 1ps tick, Coarse's `#5` would land at tick 5 and print "b" first.
`timescale 1ns / 1ps
module Top;
  Coarse c ();
  initial #1 $display("a");
endmodule

`timescale 1ns / 1ns
module Coarse;
  initial #5 $display("b");
endmodule
