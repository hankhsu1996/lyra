// The amount a delay control waits is an expression evaluated when the
// statement is reached, not only a literal: it may name a parameter, read a
// variable, or combine them arithmetically, and each procedure resumes after
// the amount its own expression evaluated to. Because the amount is fixed at
// evaluation, writing to a variable the expression read does not lengthen or
// shorten a wait already under way. Two results the expression can take are
// given meanings of their own rather than being rejected -- an unknown or
// high-impedance value is a zero delay, and a negative value is read as a
// two's-complement unsigned integer the width of a time variable, which names
// a wait no simulation of ordinary length reaches (LRM 9.4.1).
module Top;
  timeunit 1ns;
  timeprecision 1ns;

  localparam int Period = 10;
  int d = 3;
  int e = 7;
  int fixed_at_entry = 5;
  logic [7:0] unknown = 8'bxxxx_xxxx;
  logic [7:0] high_impedance = 8'bzzzz_zzzz;
  int negative = -1;

  time after_const_expr;
  time after_variable;
  time after_arithmetic;
  time after_unknown;
  time after_high_impedance;
  time after_fixed_at_entry;
  int negative_marker;

  initial begin
    #(Period / 2);
    after_const_expr = $time;
  end

  initial begin
    #d;
    after_variable = $time;
  end

  initial begin
    #((d + e) / 2);
    after_arithmetic = $time;
  end

  // The expected wake is time zero, which is also what these hold if the delay
  // never returns, so each is first set to a time the check rejects.
  initial begin
    after_unknown = 99;
    #unknown;
    after_unknown = $time;
  end

  initial begin
    after_high_impedance = 99;
    #high_impedance;
    after_high_impedance = $time;
  end

  // Reaching the delay is recorded before it, so the check can tell a
  // procedure that never resumed from one that never ran.
  initial begin
    negative_marker = 7;
    #negative;
    negative_marker = 9;
  end

  initial begin
    #fixed_at_entry;
    after_fixed_at_entry = $time;
  end

  initial begin
    #1;
    fixed_at_entry = 40;
  end

  initial begin
    #50;
    $finish;
  end

  final begin
    if (after_const_expr !== 5)
      $fatal(1, "after_const_expr was %0d, expected 5", after_const_expr);
    if (after_variable !== 3)
      $fatal(1, "after_variable was %0d, expected 3", after_variable);
    if (after_arithmetic !== 5)
      $fatal(1, "after_arithmetic was %0d, expected 5", after_arithmetic);
    if (after_unknown !== 0)
      $fatal(1, "after_unknown was %0d, expected 0", after_unknown);
    if (after_high_impedance !== 0)
      $fatal(1, "after_high_impedance was %0d, expected 0",
             after_high_impedance);
    if (negative_marker !== 7)
      $fatal(1, "negative_marker was %0d, expected 7", negative_marker);
    if (after_fixed_at_entry !== 5)
      $fatal(1, "after_fixed_at_entry was %0d, expected 5",
             after_fixed_at_entry);
    $display("All checks passed");
  end
endmodule
