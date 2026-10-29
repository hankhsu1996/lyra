// %t writes the number its operand holds as a count of the calling scope's time
// unit, in the unit, precision, suffix and field width $timeformat sets (LRM
// 20.4.3). How it writes an operand holding unknown or high-impedance bits, one
// past what a double holds exactly, one wider than 64 bits, or a negative one,
// the standard does not say. Lyra writes the operand's whole value in decimal,
// and a value with unknown or high-impedance bits as the decimal conversion
// does, x, X, z or Z (LRM 21.2.1.3). This case holds Lyra to that.
`timescale 1ns / 1ps
module Top;
  time unset;
  logic [63:0] floating;
  logic [63:0] one_unknown;
  logic [63:0] one_floating;
  logic [63:0] both;
  time past_a_double;
  logic [127:0] wide;
  int negative;

  initial begin
    floating = 'z;
    one_unknown = 64'b1x;
    one_floating = 64'b1z;
    both = {62'd0, 2'bxz};
    past_a_double = 64'd9007199254740993;
    wide = 128'd1 << 70;
    negative = -5;

    // The default display unit is the smallest precision in the design, 1ps.
    if ($sformatf("%0t", unset) != "x")
      $fatal(1, "an unset time was '%0t'", unset);
    if ($sformatf("[%t]", unset) != "[                   x]")
      $fatal(1, "an unset time in its field was '%t'", unset);
    if ($sformatf("%0t", floating) != "z")
      $fatal(1, "a floating value was '%0t'", floating);
    if ($sformatf("%0t", one_unknown) != "X")
      $fatal(1, "one unknown bit was '%0t'", one_unknown);
    if ($sformatf("%0t", one_floating) != "Z")
      $fatal(1, "one floating bit was '%0t'", one_floating);
    if ($sformatf("%0t", both) != "X")
      $fatal(1, "an unknown and a floating bit were '%0t'", both);
    if ($sformatf("%0t", past_a_double) != "9007199254740993000")
      $fatal(1, "a count past a double was '%0t'", past_a_double);
    if ($sformatf("%0t", wide) != "1180591620717411303424000")
      $fatal(1, "a count wider than 64 bits was '%0t'", wide);
    if ($sformatf("%0t", negative) != "-5000")
      $fatal(1, "a negative count was '%0t'", negative);

    $timeformat(-9, 3, " ns", 12);
    if ($sformatf("[%t]", unset) != "[        x ns]")
      $fatal(1, "an unset time with a suffix was '%t'", unset);
    if ($sformatf("[%t]", 64'd2) != "[    2.000 ns]")
      $fatal(1, "a count with fraction digits was '%t'", 64'd2);
    if ($sformatf("[%t]", negative) != "[   -5.000 ns]")
      $fatal(1, "a negative count in its unit was '%t'", negative);
    $display("All checks passed");
  end
endmodule
