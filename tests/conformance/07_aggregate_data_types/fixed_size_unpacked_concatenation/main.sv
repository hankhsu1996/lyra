// An unpacked array concatenation may target a fixed-size unpacked array: the
// items lay out left to right, an item of the element type contributing one
// element and an item that is itself an unpacked array contributing all of its
// elements in order. The resulting element count must equal the fixed-size
// target's, which the front end checks whenever every count is known and which
// otherwise holds at run time (LRM 10.10). A spread item may be a fixed-size or
// a dynamic array; both contribute their elements the same way.
module Top;
  int positional [3];
  int spread_fixed [4];
  int mixed [5];
  int from_dynamic [3];
  int two [2] = '{10, 20};
  int dyn [] = '{7, 8};

  initial begin
    positional = {1, 2, 3};
    spread_fixed = {two, two};
    mixed = {1, two, 4, 5};
    from_dynamic = {dyn, 9};
  end

  final begin
    if (positional[0] !== 1)
      $fatal(1, "positional[0] was %0d, expected 1", positional[0]);
    if (positional[1] !== 2)
      $fatal(1, "positional[1] was %0d, expected 2", positional[1]);
    if (positional[2] !== 3)
      $fatal(1, "positional[2] was %0d, expected 3", positional[2]);

    if (spread_fixed[0] !== 10)
      $fatal(1, "spread_fixed[0] was %0d, expected 10", spread_fixed[0]);
    if (spread_fixed[1] !== 20)
      $fatal(1, "spread_fixed[1] was %0d, expected 20", spread_fixed[1]);
    if (spread_fixed[2] !== 10)
      $fatal(1, "spread_fixed[2] was %0d, expected 10", spread_fixed[2]);
    if (spread_fixed[3] !== 20)
      $fatal(1, "spread_fixed[3] was %0d, expected 20", spread_fixed[3]);

    if (mixed[0] !== 1) $fatal(1, "mixed[0] was %0d, expected 1", mixed[0]);
    if (mixed[1] !== 10) $fatal(1, "mixed[1] was %0d, expected 10", mixed[1]);
    if (mixed[2] !== 20) $fatal(1, "mixed[2] was %0d, expected 20", mixed[2]);
    if (mixed[3] !== 4) $fatal(1, "mixed[3] was %0d, expected 4", mixed[3]);
    if (mixed[4] !== 5) $fatal(1, "mixed[4] was %0d, expected 5", mixed[4]);

    if (from_dynamic[0] !== 7)
      $fatal(1, "from_dynamic[0] was %0d, expected 7", from_dynamic[0]);
    if (from_dynamic[1] !== 8)
      $fatal(1, "from_dynamic[1] was %0d, expected 8", from_dynamic[1]);
    if (from_dynamic[2] !== 9)
      $fatal(1, "from_dynamic[2] was %0d, expected 9", from_dynamic[2]);
    $display("All checks passed");
  end
endmodule
