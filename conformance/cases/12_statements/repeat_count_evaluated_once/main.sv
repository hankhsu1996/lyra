// A repeat loop evaluates its count expression once, before the loop starts,
// so changing any part of that expression from inside the body has no effect
// on how many passes run (LRM 12.7.2).
module Top;
  int n;
  int iters;
  int a;
  int b;
  int sum_iters;

  initial begin
    n = 3;
    iters = 0;
    repeat (n) begin
      iters = iters + 1;
      n = n - 1;
    end

    a = 2;
    b = 1;
    sum_iters = 0;
    repeat (a + b) begin
      sum_iters = sum_iters + 1;
      a = 0;
      b = 0;
    end
  end

  final begin
    if (iters !== 3) $fatal(1, "iters was %0d, expected 3", iters);
    if (n !== 0) $fatal(1, "n was %0d, expected 0", n);
    if (sum_iters !== 3) $fatal(1, "sum_iters was %0d, expected 3", sum_iters);
    $display("All checks passed");
  end
endmodule
