`timescale 1ns / 1ns
module Test;
  bit clk = 0;
  initial repeat (6) #5 clk = ~clk;
  always @(posedge clk) begin
    static int count = 0;
    count = count + 1;
    $display("count=%0d", count);
  end
endmodule
