module Top;
  logic clk;
  int   d;
  int   q;
  initial begin
    clk = 0;
    d   = 42;
    q   = 0;
    @(clk);
    q <= d;
  end
  initial begin
    #5;
    clk = 1;
  end
endmodule
