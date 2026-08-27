// A repeat loop executes its statement a fixed number of times, given by its
// count expression, whether that expression is a literal or a variable and
// whether the statement is a block or a single assignment (LRM 12.7.2).
module Top;
  int i;
  int sum;
  int x;
  int n;
  int k;
  int series;

  initial begin
    i = 0;
    sum = 0;
    repeat (5) begin
      sum = sum + i;
      i = i + 1;
    end

    x = 0;
    repeat (4) x = x + 1;

    n = 6;
    k = 1;
    series = 0;
    repeat (n) begin
      series = series + k;
      k = k + 1;
    end
  end

  final begin
    if (i !== 5) $fatal(1, "i was %0d, expected 5", i);
    if (sum !== 10) $fatal(1, "sum was %0d, expected 10", sum);
    if (x !== 4) $fatal(1, "x was %0d, expected 4", x);
    if (series !== 21) $fatal(1, "series was %0d, expected 21", series);
    $display("All checks passed");
  end
endmodule
