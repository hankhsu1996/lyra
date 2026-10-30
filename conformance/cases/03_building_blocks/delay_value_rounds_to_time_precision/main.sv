// A delay value is expressed in the time unit of the design element it sits
// in, and the element's time precision says how that value is rounded before
// simulation uses it. Where the precision is finer than the unit, a delay
// keeps that many decimal places rather than being taken to a whole unit, and
// it is rounded rather than truncated -- the standard's own example is that a
// 1ns element accurate to 100ps waits 2.8ns when asked for 2.75. A value the
// precision cannot express at all rounds to the nearest amount it can, which
// for a value below half a precision step is no delay. Each delay is rounded
// where it is written, so repeating one accumulates the rounded amount
// (LRM 3.14.1).
module Top;
  timeunit 1ns;
  timeprecision 100ps;

  time after_half_step_up;
  time after_below_the_unit;
  time after_below_the_precision;

  // Ten roundings, so the rounded and the truncated amounts land a whole time
  // unit apart and $time separates them without reading a fraction back.
  initial begin
    repeat (10) #2.75;
    after_half_step_up = $time;
  end

  initial begin
    repeat (10) #0.06;
    after_below_the_unit = $time;
  end

  // Rounding to no delay leaves this at time zero, which is what it would also
  // hold had the procedure never resumed, so it starts at a time the check
  // rejects.
  initial begin
    after_below_the_precision = 99;
    repeat (10) #0.04;
    after_below_the_precision = $time;
  end

  final begin
    if (after_half_step_up !== 28)
      $fatal(1, "after_half_step_up was %0d, expected 28", after_half_step_up);
    if (after_below_the_unit !== 1)
      $fatal(1, "after_below_the_unit was %0d, expected 1",
             after_below_the_unit);
    if (after_below_the_precision !== 0)
      $fatal(1, "after_below_the_precision was %0d, expected 0",
             after_below_the_precision);
    $display("All checks passed");
  end
endmodule
