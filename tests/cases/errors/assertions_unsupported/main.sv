module Top;
  logic clk = 1'b0;
  logic a = 1'b0;
  logic b = 1'b1;

  assert property (@(posedge clk) a |-> b);
endmodule
