module Top;
  bit clk;
  int a;
  int b;

  always    @(posedge clk) a <= a + 1;
  always_ff @(posedge clk) b <= b + 1;

  initial begin
    clk = 0;
    a = 0;
    b = 0;
    #5 clk = 1; #5 clk = 0;
    #5 clk = 1; #5 clk = 0;
    #5 clk = 1; #5 clk = 0;
    $finish;
  end
endmodule
