// A constructor takes its arguments by the conventions every other subroutine
// call takes them by, so it may declare an output or a ref formal as freely as
// any method: the output is copied back to its actual when construction
// completes, and the ref aliases its actual while the body runs, so a
// constructor can both build the object and hand values to the statement that
// asked for it. Two constructions sharing one ref actual therefore see each
// other's writes, in the order the source constructed them
// (LRM 8.7, 13.5, 13.5.2).
module Top;
  class Counter;
    int id;

    function new(input int seed, output int assigned, ref int minted);
      minted = minted + 1;
      id = seed + minted;
      assigned = id;
    endfunction
  endclass

  Counter first;
  Counter second;

  int minted;
  int first_assigned;
  int second_assigned;

  initial begin
    minted = 0;
    first_assigned = -1;
    second_assigned = -1;
    first = new(10, first_assigned, minted);
    second = new(20, second_assigned, minted);
  end

  final begin
    if (minted !== 2) $fatal(1, "minted was %0d, expected 2", minted);
    if (first_assigned !== 11)
      $fatal(1, "first_assigned was %0d, expected 11", first_assigned);
    if (first.id !== 11)
      $fatal(1, "first.id was %0d, expected 11", first.id);
    if (second_assigned !== 22)
      $fatal(1, "second_assigned was %0d, expected 22", second_assigned);
    if (second.id !== 22)
      $fatal(1, "second.id was %0d, expected 22", second.id);
    $display("All checks passed");
  end
endmodule
