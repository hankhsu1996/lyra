// $random returns a new 32-bit random number each time it is called, as a
// signed integer that may be positive or negative (LRM 20.14.1). Called with no
// seed it names no stream of its own, and the standard fixes no values for it,
// so what a case can hold it to is the shape of what it returns rather than
// which numbers come out: 32 bits wide, the range LRM 20.14.1 gives as its own
// example, and that the numbers move at all -- an implementation answering with
// one number forever is not returning a new one each call.
module Top;
  int drawn;
  int previous;
  int i;
  bit moved;

  final begin
    if ($bits($random) !== 32)
      $fatal(1, "$random is %0d bits wide, expected 32", $bits($random));

    // LRM 20.14.1 Example 1 states the range this remainder falls in. The draw
    // before it is a value the range rejects, so a call that answered with
    // nothing at all would be caught here rather than passing.
    for (i = 0; i < 20; i = i + 1) begin
      drawn = 100;
      drawn = $random % 60;
      if (drawn > 59 || drawn < -59)
        $fatal(1, "$random %% 60 was %0d, expected -59 through 59", drawn);
    end

    moved = 0;
    previous = $random;
    for (i = 0; i < 20; i = i + 1) begin
      drawn = previous;
      drawn = $random;
      if (drawn !== previous) moved = 1;
      previous = drawn;
    end
    if (moved !== 1)
      $fatal(1, "twenty-one $random calls all answered %0d", previous);

    $display("All checks passed");
  end
endmodule
