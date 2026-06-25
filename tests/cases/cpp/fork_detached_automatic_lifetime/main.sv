`timescale 1ns / 1ns
module Test;
  initial begin
    automatic int k = 7;
    fork
      #10 $display("branch k=%0d", k);
    join_none
    k = 99;
    #5;
    $display("parent done");
  end
endmodule
