// A repeat loop whose count evaluates to zero executes its statement no
// times, because the count decides the number of passes before the first one
// rather than after it (LRM 12.7.2). A target the body would have advanced is
// therefore left holding what it held before the loop.
module Top;
  int after_literal_zero;
  int n;
  int after_variable_zero;

  initial begin
    after_literal_zero = 5;
    repeat (0) after_literal_zero = after_literal_zero + 1;

    n = 0;
    after_variable_zero = 9;
    repeat (n) after_variable_zero = after_variable_zero + 1;
  end

  final begin
    if (after_literal_zero !== 5)
      $fatal(1, "after_literal_zero was %0d, expected 5", after_literal_zero);
    if (after_variable_zero !== 9)
      $fatal(1, "after_variable_zero was %0d, expected 9",
             after_variable_zero);
    $display("All checks passed");
  end
endmodule
