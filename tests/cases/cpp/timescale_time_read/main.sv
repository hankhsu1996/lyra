// $time / $stime (LRM 20.3) return the current simulation time scaled to the
// calling scope's time unit. Top is 1ns/1ps, so the design-global tick is 1ps
// and `#5` lands at 5000 ticks; $time scales that back to the 1ns unit
// (5000 / 10^(unit(-9) - global(-12)) = 5000 / 1000 = 5). $stime is the 32-bit
// twin of the same value.
`timescale 1ns / 1ps
module Top;
  time t;
  int st;
  initial begin
    #5;
    t = $time;
    st = $stime;
  end
endmodule
