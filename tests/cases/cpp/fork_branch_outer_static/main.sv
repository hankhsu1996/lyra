`timescale 1ns / 1ns
module Test;
  initial begin
    static int x = 5;
    fork
      #10 $display("[%0d] read x=%0d", $time, x);
      #20 x = 7;
    join
    $display("[%0d] after join x=%0d", $time, x);
  end
endmodule
