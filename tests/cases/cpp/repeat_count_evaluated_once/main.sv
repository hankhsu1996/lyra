module Top;
  initial begin
    int n = 3;
    int iters = 0;
    repeat (n) begin
      iters = iters + 1;
      n = n - 1;
    end
    $display("iters=%0d final=%0d", iters, n);
  end
endmodule
