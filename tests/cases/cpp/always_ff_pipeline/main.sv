module Top;
  bit clk;
  int d;
  int q1;
  int q2;

  always_ff @(posedge clk) begin
    q1 <= d;
    q2 <= q1;
  end

  initial begin
    clk = 0;
    d = 10;
    #5 clk = 1; #5 clk = 0;
    #5 clk = 1; #5 clk = 0;
    $finish;
  end
endmodule
