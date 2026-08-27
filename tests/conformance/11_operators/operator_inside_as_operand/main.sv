// The single-bit result the inside operator produces is an operand like any
// other, so it takes part in arithmetic and decides a conditional operator
// wherever the scan reached a determined answer (LRM 11.4.13).
module Top;
  int v;
  int sum_of_results = -1;
  int ternary_taken = -1;
  int ternary_not_taken = -1;

  initial begin
    v = 3;
    sum_of_results = (v inside {1, 2, 3}) + (v inside {4, 5, 6});
    v = 5;
    ternary_taken = (v inside {[1:10]}) ? 100 : 200;
    v = 50;
    ternary_not_taken = (v inside {[1:10]}) ? 100 : 200;
  end

  final begin
    if (sum_of_results !== 1)
      $fatal(1, "the two results summed to %0d, expected 1", sum_of_results);
    if (ternary_taken !== 100)
      $fatal(1, "a matching inside chose %0d, expected 100", ternary_taken);
    if (ternary_not_taken !== 200)
      $fatal(1, "a missing inside chose %0d, expected 200", ternary_not_taken);
    $display("All checks passed");
  end
endmodule
