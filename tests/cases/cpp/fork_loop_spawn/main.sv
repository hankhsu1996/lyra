`timescale 1ns / 1ns
module Test;
  initial
    for (int i = 0; i < 3; i++)
      fork
        automatic int k = i;
        #5 $display("k=%0d", k);
      join_none
endmodule
