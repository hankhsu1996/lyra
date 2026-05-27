module Top;
  bit clk;
  int d;
  int q;

  always_ff @(posedge clk) q <= d;

  initial begin
    clk = 0;
    d = 42;
    #5 clk = 1;
    #5 clk = 0;
    d = 99;
    #5 clk = 1;
    #5 clk = 0;
    $finish;
  end
endmodule
