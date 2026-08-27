// The case expression is evaluated once on each execution of the case
// statement, and before any of that execution's case item expressions
// (LRM 12.5), so a case statement reached repeatedly as a loop body selects on
// the value it is handed each time rather than staying with the first.
module Top;
  int zero_taken;
  int pair_taken;
  int three_taken;
  int default_taken;

  initial begin
    zero_taken = 0;
    pair_taken = 0;
    three_taken = 0;
    default_taken = 0;
    for (int i = 0; i < 5; i++) begin
      case (i)
        0:    zero_taken = zero_taken + 1;
        1, 2: pair_taken = pair_taken + 1;
        3:    three_taken = three_taken + 1;
        default: default_taken = default_taken + 1;
      endcase
    end
  end

  final begin
    if (zero_taken !== 1)
      $fatal(1, "zero_taken was %0d, expected 1", zero_taken);
    if (pair_taken !== 2)
      $fatal(1, "pair_taken was %0d, expected 2", pair_taken);
    if (three_taken !== 1)
      $fatal(1, "three_taken was %0d, expected 1", three_taken);
    if (default_taken !== 1)
      $fatal(1, "default_taken was %0d, expected 1", default_taken);
    $display("All checks passed");
  end
endmodule
