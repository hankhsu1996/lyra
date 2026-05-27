module Top;
  logic clk;
  logic rst_n;
  int   wake_count;
  initial begin
    clk = 0;
    rst_n = 1;
    wake_count = 0;
    @(posedge clk or negedge rst_n);
    wake_count = 1;
    @(posedge clk or negedge rst_n);
    wake_count = 2;
  end
  initial begin
    #5;
    rst_n = 0;
    #5;
    rst_n = 1;
    #5;
    clk = 1;
  end
endmodule
