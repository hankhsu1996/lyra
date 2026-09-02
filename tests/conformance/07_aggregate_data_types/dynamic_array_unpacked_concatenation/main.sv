// An unpacked array concatenation builds a dynamic array value from a
// comma-separated list: an operand of the element type contributes one element,
// an operand that is itself an unpacked array contributes all of its elements
// in order, and the results lay out left to right. The list with no items is
// the dynamic array with no elements. Assigning such a concatenation back to the
// array that appears inside it grows or reseeds it, and a fixed-size array
// operand spreads its elements the same way a dynamic one does (LRM 10.10).
module Top;
  int grown [] = '{1, 2, 3};
  int prepended [] = '{1, 2, 3};
  int low [] = '{10, 20};
  int high [] = '{30, 40};
  int joined [];
  int spliced [];
  int emptied [] = '{7, 8};
  int reseeded [] = '{7, 8};
  int fixed_src [2] = '{100, 200};
  int from_fixed [];

  initial begin
    grown = {grown, 4};
    prepended = {0, prepended};
    joined = {low, high};
    spliced = {low, 25, high};
    emptied = {};
    reseeded = {};
    reseeded = {reseeded, 5};
    from_fixed = {fixed_src, 300};
  end

  final begin
    if (grown.size() !== 4)
      $fatal(1, "grown.size() was %0d, expected 4", grown.size());
    if (grown[0] !== 1) $fatal(1, "grown[0] was %0d, expected 1", grown[0]);
    if (grown[3] !== 4) $fatal(1, "grown[3] was %0d, expected 4", grown[3]);

    if (prepended.size() !== 4)
      $fatal(1, "prepended.size() was %0d, expected 4", prepended.size());
    if (prepended[0] !== 0)
      $fatal(1, "prepended[0] was %0d, expected 0", prepended[0]);
    if (prepended[3] !== 3)
      $fatal(1, "prepended[3] was %0d, expected 3", prepended[3]);

    if (joined.size() !== 4)
      $fatal(1, "joined.size() was %0d, expected 4", joined.size());
    if (joined[0] !== 10) $fatal(1, "joined[0] was %0d, expected 10", joined[0]);
    if (joined[3] !== 40) $fatal(1, "joined[3] was %0d, expected 40", joined[3]);

    if (spliced.size() !== 5)
      $fatal(1, "spliced.size() was %0d, expected 5", spliced.size());
    if (spliced[2] !== 25)
      $fatal(1, "spliced[2] was %0d, expected 25", spliced[2]);
    if (spliced[3] !== 30)
      $fatal(1, "spliced[3] was %0d, expected 30", spliced[3]);

    if (emptied.size() !== 0)
      $fatal(1, "emptied.size() was %0d, expected 0", emptied.size());

    if (reseeded.size() !== 1)
      $fatal(1, "reseeded.size() was %0d, expected 1", reseeded.size());
    if (reseeded[0] !== 5)
      $fatal(1, "reseeded[0] was %0d, expected 5", reseeded[0]);

    if (from_fixed.size() !== 3)
      $fatal(1, "from_fixed.size() was %0d, expected 3", from_fixed.size());
    if (from_fixed[0] !== 100)
      $fatal(1, "from_fixed[0] was %0d, expected 100", from_fixed[0]);
    if (from_fixed[1] !== 200)
      $fatal(1, "from_fixed[1] was %0d, expected 200", from_fixed[1]);
    if (from_fixed[2] !== 300)
      $fatal(1, "from_fixed[2] was %0d, expected 300", from_fixed[2]);
    $display("All checks passed");
  end
endmodule
