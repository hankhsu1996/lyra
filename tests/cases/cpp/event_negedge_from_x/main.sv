// LRM 9.4.2 Table 9-2: negedge fires on 1->{0,x,z} and {x,z}->0.
module Top;
  logic clk_a, clk_b;
  int fired_on_1_to_x, fired_on_x_to_0;

  initial begin
    fired_on_1_to_x = 0;
    fired_on_x_to_0 = 0;
    clk_a = 1'b1;
    clk_b = 1'bx;
    #1;
    clk_a = 1'bx;  // 1 -> x: should fire negedge
    clk_b = 1'b0;  // x -> 0: should fire negedge
    #1;
  end

  always @(negedge clk_a) fired_on_1_to_x = 1;
  always @(negedge clk_b) fired_on_x_to_0 = 1;
endmodule
