// $realtime (LRM 20.3.3) scales the current time to the calling scope's unit
// like $time but returns a real, preserving any fractional part. Top is
// 1ns/1ps, so `#5` lands at 5000 global ticks and $realtime reads
// 5000 / 1000.0 = 5.0, printed as "5" by %g.
`timescale 1ns / 1ps
module Top;
  initial begin
    #5;
    $display("%g", $realtime);
  end
endmodule
