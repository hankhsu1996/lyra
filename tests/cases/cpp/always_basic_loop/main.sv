module Top;
  bit clk;
  int iters;

  initial begin
    clk = 0;
    iters = 0;
  end

  always @(posedge clk) begin
    iters = iters + 1;
    if (iters == 3) $finish;
  end

  initial begin
    #1; clk = 1; #1; clk = 0;
    #1; clk = 1; #1; clk = 0;
    #1; clk = 1; #1; clk = 0;
    #1; clk = 1;
  end
endmodule
