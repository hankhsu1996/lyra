module Top;
  bit clk;
  bit rst_n;
  int q;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) q <= 0;
    else        q <= q + 1;
  end

  initial begin
    clk = 0;
    rst_n = 0;
    #1 rst_n = 1;
    #5 clk = 1; #5 clk = 0;
    #5 clk = 1; #5 clk = 0;
    #5 clk = 1; #5 clk = 0;
    $finish;
  end
endmodule
