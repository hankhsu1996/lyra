// A time literal is a realtime value scaled to the current time unit, so a
// literal written in a coarser unit denotes many of them and one written in a
// finer unit denotes a fraction of one. What it scales to is a number like any
// other, so it carries its fraction and arithmetic composes on it (LRM 5.8).
`timescale 1ns / 1ps
module Top;
  realtime same_unit = -1.0;
  realtime coarser_unit = -1.0;
  realtime finer_unit = -1.0;
  realtime fixed_point = -1.0;
  realtime summed = -1.0;

  localparam realtime kParameter = 250ps;

  initial begin
    same_unit = 5ns;
    coarser_unit = 5us;
    finer_unit = 1500ps;
    fixed_point = 2.5ns;
    summed = 1ns + 500ps;
  end

  final begin
    if (same_unit != 5.0)
      $fatal(1, "5ns was %f, expected 5.0", same_unit);
    if (coarser_unit != 5000.0)
      $fatal(1, "5us was %f, expected 5000.0", coarser_unit);
    if (finer_unit != 1.5)
      $fatal(1, "1500ps was %f, expected 1.5", finer_unit);
    if (fixed_point != 2.5)
      $fatal(1, "2.5ns was %f, expected 2.5", fixed_point);
    if (summed != 1.5)
      $fatal(1, "1ns + 500ps was %f, expected 1.5", summed);
    if (kParameter != 0.25)
      $fatal(1, "250ps as a parameter was %f, expected 0.25", kParameter);
    $display("All checks passed");
  end
endmodule
