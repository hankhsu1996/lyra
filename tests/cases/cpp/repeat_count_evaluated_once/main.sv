module Top;
  int n;
  int iters;
  initial begin
    n = 3;
    iters = 0;
    repeat (n) begin
      iters = iters + 1;
      n = n - 1;
    end
  end
endmodule
