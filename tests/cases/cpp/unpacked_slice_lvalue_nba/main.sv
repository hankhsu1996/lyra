module Top;
  logic clk = 0;
  int a [6] = '{10, 20, 30, 40, 50, 60};
  int src [3] = '{77, 88, 99};

  always @(posedge clk) begin
    a[1 +: 3] <= src;
  end

  initial begin
    #1 clk = 1;
    #1 clk = 0;
  end
endmodule
