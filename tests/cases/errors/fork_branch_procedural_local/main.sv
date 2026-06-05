`timescale 1ns / 1ns
module Test;
  initial begin
    int x = 5;
    fork
      #10 $display("%0d", x);
    join
  end
endmodule
