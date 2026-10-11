// A formal argument of an imported subroutine carries values across the
// foreign boundary according to its direction: an input is copied in and the
// actual is left untouched, an output is copied out into the actual when the
// call returns, and an inout is copied in and then back out. A function may
// carry output and inout arguments alongside its result (LRM 35.5.1.2, 35.5.5,
// 35.6.1, 35.6.2). A string formal follows the same three directions, but what
// the direction applies to is the pointer rather than the characters, so an
// output or inout string is reached through one more level of indirection
// (LRM Annex H.8.10). An actual of another type than its formal takes the
// formal's value, and gives the formal its own, as an assignment between the
// two would (LRM 13.5).
module Top;
  import "DPI-C" function int mix(input int high, input int low);
  import "DPI-C" function void split(
      input int combined, output int high, output int low);
  import "DPI-C" function void fold_in(inout int acc, input int addend);
  import "DPI-C" function int divide(
      input int numerator, input int denominator, output int remainder);
  import "DPI-C" function int char_at(input string text, input int index);
  import "DPI-C" function void name_of(input int code, output string text);
  import "DPI-C" function void bracket(inout string text);
  import "DPI-C" function void halve(inout real value);

  int mixed;
  int passed_in;
  int high;
  int low;
  int acc;
  int quotient;
  int remainder;
  int letter;
  string chosen;
  string wrapped;
  real halved;
  logic [3:0] narrow_high;
  real real_low;
  logic [7:0] unknown_acc;
  int halved_and_rounded;

  initial begin
    // Each actual an argument writes into starts at a value the foreign side
    // never produces, so having been written is distinguishable from having
    // held the answer all along.
    passed_in = 6;
    mixed = mix(passed_in, 7);

    high = -1;
    low = -2;
    split(1234, high, low);

    acc = 5;
    fold_in(acc, 3);

    remainder = -3;
    quotient = divide(47, 5, remainder);

    letter = char_at("abcdef", 3);

    chosen = "unset";
    name_of(2, chosen);

    wrapped = "core";
    bracket(wrapped);

    halved = 25.25;
    halve(halved);

    narrow_high = 4'hx;
    real_low = -2.0;
    split(2834, narrow_high, real_low);

    unknown_acc = 8'b0000_01x1;
    fold_in(unknown_acc, 3);

    halved_and_rounded = 25;
    halve(halved_and_rounded);
  end

  final begin
    if (mixed !== 6007) $fatal(1, "mixed was %0d, expected 6007", mixed);
    if (passed_in !== 6)
      $fatal(1, "passed_in was %0d, expected 6", passed_in);
    if (high !== 12) $fatal(1, "high was %0d, expected 12", high);
    if (low !== 34) $fatal(1, "low was %0d, expected 34", low);
    if (acc !== 53) $fatal(1, "acc was %0d, expected 53", acc);
    if (quotient !== 9) $fatal(1, "quotient was %0d, expected 9", quotient);
    if (remainder !== 2)
      $fatal(1, "remainder was %0d, expected 2", remainder);
    if (letter !== 100) $fatal(1, "letter was %0d, expected 100", letter);
    if (chosen != "beta")
      $fatal(1, "chosen was '%s', expected 'beta'", chosen);
    if (wrapped != "[core]")
      $fatal(1, "wrapped was '%s', expected '[core]'", wrapped);
    if (halved != 12.625)
      $fatal(1, "halved was %f, expected 12.625", halved);
    if (narrow_high !== 4'hC)
      $fatal(1, "narrow_high was %h, expected c", narrow_high);
    if (real_low != 34.0)
      $fatal(1, "real_low was %f, expected 34.0", real_low);
    if (unknown_acc !== 8'd53)
      $fatal(1, "unknown_acc was %0d, expected 53", unknown_acc);
    if (halved_and_rounded !== 13)
      $fatal(1, "halved_and_rounded was %0d, expected 13", halved_and_rounded);
    $display("All checks passed");
  end
endmodule
