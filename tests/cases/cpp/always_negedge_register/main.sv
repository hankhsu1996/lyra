module Top;
  bit clk;
  int d;
  int q;

  always_ff @(negedge clk) q <= d;

  initial begin
    clk = 1;
    d = 11;
    #5 clk = 0; #5 clk = 1;
    d = 77;
    #5 clk = 0; #5 clk = 1;
    $finish;
  end
endmodule
