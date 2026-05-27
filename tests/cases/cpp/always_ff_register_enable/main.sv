module Top;
  bit clk;
  bit en;
  int d;
  int q;

  always_ff @(posedge clk) begin
    if (en) q <= d;
  end

  initial begin
    clk = 0;
    en = 1;
    d = 42;
    #5 clk = 1;
    #5 clk = 0;
    d = 99;
    #5 clk = 1;
    #5 clk = 0;
    en = 0;
    d = 7;
    #5 clk = 1;
    #5 clk = 0;
    $finish;
  end
endmodule
