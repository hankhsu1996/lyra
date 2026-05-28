// LRM 9.4.2 Table 9-2: posedge fires on 0->{1,x,z} and {x,z}->1. This test
// covers the 4-state corners (0->x and x->1) that pure-binary 0->1 edge
// detection misses.
module Top;
  logic clk_a, clk_b;
  int fired_on_0_to_x, fired_on_x_to_1;

  initial begin
    fired_on_0_to_x = 0;
    fired_on_x_to_1 = 0;
    clk_a = 1'b0;
    clk_b = 1'bx;
    #1;
    clk_a = 1'bx;  // 0 -> x: should fire posedge
    clk_b = 1'b1;  // x -> 1: should fire posedge
    #1;
  end

  always @(posedge clk_a) fired_on_0_to_x = 1;
  always @(posedge clk_b) fired_on_x_to_1 = 1;
endmodule
