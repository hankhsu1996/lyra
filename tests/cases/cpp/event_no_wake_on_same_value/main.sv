module Top;
  logic clk;
  int   wake_count;
  initial begin
    clk = 0;
    wake_count = 0;
    repeat (3) begin
      @(clk);
      wake_count = wake_count + 1;
    end
  end
  initial begin
    #5;
    clk = 0;
    #5;
    clk = 1;
    #5;
    clk = 1;
    #5;
    clk = 0;
    #5;
    clk = 1;
  end
endmodule
