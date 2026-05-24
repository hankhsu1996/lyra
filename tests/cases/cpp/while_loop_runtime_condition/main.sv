module Top;
  int n;
  int iters;
  initial begin
    n = 1;
    iters = 0;
    while (n != 256) begin
      n = n * 2;
      iters = iters + 1;
    end
  end
endmodule
