module Top;
  logic clk = 0;
  int a [6] = '{10, 20, 30, 40, 50, 60};
  int src [3] = '{77, 88, 99};
  int p0, p1, p2, p3, p4, p5;

  always @(posedge clk) begin
    a[1 +: 3] <= src;
  end

  initial begin
    #1 clk = 1;
    #1 clk = 0;
    p0 = a[0];
    p1 = a[1];
    p2 = a[2];
    p3 = a[3];
    p4 = a[4];
    p5 = a[5];
  end
endmodule
