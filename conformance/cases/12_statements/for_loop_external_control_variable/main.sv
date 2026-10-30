// A for-loop's control variable may be declared before the loop, in which case
// the for_initialization is an ordinary assignment to it and the variable is
// still in scope after the loop. The step assignment runs after each pass and
// the control expression is evaluated after that, so what the variable is left
// holding is the first value for which the expression was false (LRM 12.7.1).
module Top;
  int i;
  int sum;

  initial begin
    sum = 0;
    for (i = 0; i < 5; i = i + 1) sum = sum + i;
  end

  final begin
    if (sum !== 10) $fatal(1, "sum was %0d, expected 10", sum);
    if (i !== 5) $fatal(1, "i was %0d, expected 5", i);
    $display("All checks passed");
  end
endmodule
