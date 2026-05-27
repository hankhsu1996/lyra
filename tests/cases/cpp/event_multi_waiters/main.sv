module Top;
  logic clk;
  int   a;
  int   b;
  initial begin
    clk = 0;
    a = 0;
    b = 0;
  end
  initial begin
    @(clk);
    a = 1;
  end
  initial begin
    @(clk);
    b = 2;
  end
  initial begin
    #5;
    clk = 1;
  end
endmodule
