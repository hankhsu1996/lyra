`timescale 1ns / 1ns
module Test;
  initial begin
    int x = 1;
    fork
      #5 $display("x=%0d", x);
    join_none
  end
endmodule
