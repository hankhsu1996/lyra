// An imported function may be declared pure, which asserts that its result
// depends solely on the values of its input arguments and that it has no side
// effect whatsoever. A tool is then free to drop a call whose result is not
// needed, or to answer one from the result of an earlier call, but only where
// the earlier call had the same input values -- so calls differing in any
// argument, or in the order of two arguments, still answer differently. Only a
// nonvoid function with no output and no inout argument may be declared pure
// (LRM 35.5.1.3, 35.5.2).
module Top;
  import "DPI-C" pure function int blend(input int a, input int b);

  int first;
  int second;
  int repeated;
  int swapped;

  initial begin
    first = blend(3, 5);
    second = blend(4, 5);
    repeated = blend(3, 5);
    swapped = blend(5, 3);
  end

  final begin
    if (first !== 305) $fatal(1, "first was %0d, expected 305", first);
    if (second !== 405) $fatal(1, "second was %0d, expected 405", second);
    if (repeated !== 305)
      $fatal(1, "repeated was %0d, expected 305", repeated);
    if (swapped !== 503) $fatal(1, "swapped was %0d, expected 503", swapped);
    $display("All checks passed");
  end
endmodule
