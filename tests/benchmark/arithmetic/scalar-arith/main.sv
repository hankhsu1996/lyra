// @measure: run
// @work: iteration
//
// A tight loop of 32-bit scalar arithmetic, with no array access, no wide
// types, and nothing crossing between processes. Each iteration depends on the
// last, so the loop cannot be folded into a closed form.
module Top;
  initial begin
    int num_iters;
    int a, b, c;
    longint sum;

    if (!$value$plusargs("work=%d", num_iters)) num_iters = 1000;

    a = 32'h1234_5678;
    b = 32'hDEAD_BEEF;
    c = 32'h0BAD_CAFE;
    sum = 0;

    for (int i = 0; i < num_iters; i++) begin
      a = a + b;
      b = b ^ (a >> 3);
      c = c + (a * 7);
      sum = sum + longint'(a) + longint'(b) + longint'(c);
    end

    $display("scalar-arith done: sum=%0d", sum);
    $finish;
  end
endmodule
