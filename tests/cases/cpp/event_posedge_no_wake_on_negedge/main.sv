module Top;
  logic clk;
  int   fired;
  initial begin
    clk = 1;
    fired = 0;
    @(posedge clk);
    fired = 1;
  end
  initial begin
    #5;
    clk = 0;
    #5;
    $finish;
  end
endmodule
