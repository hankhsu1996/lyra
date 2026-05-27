module Top;
  bit clk;
  int edges;

  initial begin
    clk = 0;
    edges = 0;
  end

  always #5 clk = ~clk;

  initial begin
    repeat (4) @(posedge clk) edges = edges + 1;
    $finish;
  end
endmodule
