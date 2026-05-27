module Top;
  logic clk;
  int   pos_count;
  int   neg_count;
  initial begin
    clk = 0;
    pos_count = 0;
    neg_count = 0;
    @(edge clk);
    pos_count = 1;
    @(edge clk);
    neg_count = 1;
  end
  initial begin
    #5;
    clk = 1;
    #5;
    clk = 0;
  end
endmodule
