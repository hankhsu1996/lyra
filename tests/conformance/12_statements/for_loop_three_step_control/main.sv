// A for-loop runs its initialization once, evaluates its control expression
// before every pass and leaves the loop the first time that expression is
// false, and runs its step assignments after each pass. How many passes a loop
// makes therefore follows from those three parts alone, whether the control
// variable counts up or down, and a loop whose expression is false at entry
// does not execute its body at all (LRM 12.7.1).
module Top;
  int up;
  int from_five;
  int down;
  int no_pass;

  initial begin
    // Each pass appends the control variable's value to a decimal trace, so
    // the digits say which values the body ran on and in what order. The
    // trace starts at 1 rather than 0 so that a pass over the value zero
    // shows in it as well.
    up = 1;
    for (int i = 0; i < 5; i = i + 1) up = up * 10 + i;

    from_five = 1;
    for (int i = 5; i < 10; i = i + 1) from_five = from_five * 10 + i;

    down = 1;
    for (int i = 4; i > 0; i = i - 1) down = down * 10 + i;

    no_pass = 99;
    for (int i = 0; i < 0; i = i + 1) no_pass = no_pass + 1;
  end

  final begin
    if (up !== 101234) $fatal(1, "up was %0d, expected 101234", up);
    if (from_five !== 156789)
      $fatal(1, "from_five was %0d, expected 156789", from_five);
    if (down !== 14321) $fatal(1, "down was %0d, expected 14321", down);
    if (no_pass !== 99) $fatal(1, "no_pass was %0d, expected 99", no_pass);
    $display("All checks passed");
  end
endmodule
