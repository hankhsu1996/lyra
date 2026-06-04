// %t formats a time value against the design-wide $timeformat settings (LRM
// 21.2.1.3 / 20.4.3), in $display and $sformatf alike (LRM 21.3.3). Before any
// $timeformat call the display unit defaults to the design-global precision
// (LRM Table 20-3): here 1ps, so $time (read in the 1ns scope unit, = 5) scales
// up by 10^3 to 5000 and pads to the default field width of 20. After
// $timeformat(-9, 2, " ns", 0) the display unit is ns, so the same time prints
// "5.00 ns" -- and $sformatf produces the identical text, proving the
// string-format path reads the same design-wide state.
`timescale 1ns / 1ps
module Top;
  string s;
  initial begin
    #5;
    $display("[%t]", $time);
    $timeformat(-9, 2, " ns", 0);
    $display("%t", $time);
    s = $sformatf("%t", $time);
    $display("s=%s", s);
  end
endmodule
