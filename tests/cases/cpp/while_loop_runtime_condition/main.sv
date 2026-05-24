module Top;
  initial begin
    int n = 1;
    int iters = 0;
    while (n != 256) begin
      n = n * 2;
      iters = iters + 1;
    end
    $display("n=%0d iters=%0d", n, iters);
  end
endmodule
