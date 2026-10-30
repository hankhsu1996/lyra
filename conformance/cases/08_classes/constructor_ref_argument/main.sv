// A constructor's `ref` formal aliases its actual while the body runs, as any
// subroutine's does: what the body writes through it lands in the actual, and
// what it reads is the actual as it stands, so two constructions sharing one
// actual see each other's writes in the order the source made them, whether
// the actual is a variable or a property of another object (LRM 8.7, 13.5.2).
// A derived constructor hands its own `ref` formal on to its base's as a call
// does, so the base writes the derived constructor's actual; and a `const ref`
// formal reads the actual as it stands, here after the base wrote it.
module Top;
  class Tally;
    int count;
  endclass

  class Counter;
    int id;

    function new(input int seed, ref int minted);
      minted = minted + 1;
      id = seed + minted;
    endfunction
  endclass

  class Labelled extends Counter;
    int label;

    function new(ref int minted, const ref int step);
      super.new(40, minted);
      label = minted * step;
    endfunction
  endclass

  int minted;
  int step;
  Counter first;
  Counter second;
  Counter third;
  Tally tally;
  Labelled fourth;

  initial begin
    minted = 0;
    first = new(10, minted);
    second = new(20, minted);
    tally = new;
    tally.count = 5;
    third = new(30, tally.count);
    step = 100;
    fourth = new(minted, step);
  end

  final begin
    if (minted !== 3) $fatal(1, "minted was %0d, expected 3", minted);
    if (first.id !== 11) $fatal(1, "first.id was %0d, expected 11", first.id);
    if (second.id !== 22) $fatal(1, "second.id was %0d, expected 22", second.id);
    if (tally.count !== 6) $fatal(1, "tally.count was %0d, expected 6", tally.count);
    if (third.id !== 36) $fatal(1, "third.id was %0d, expected 36", third.id);
    if (fourth.id !== 43) $fatal(1, "fourth.id was %0d, expected 43", fourth.id);
    if (fourth.label !== 300)
      $fatal(1, "fourth.label was %0d, expected 300", fourth.label);
    $display("All checks passed");
  end
endmodule
