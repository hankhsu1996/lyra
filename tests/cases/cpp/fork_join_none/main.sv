`timescale 1ns / 1ns
module Test;
  initial begin
    fork
      #10 $display("[%0d] branch A", $time);
    join_none
    $display("[%0d] after join_none", $time);
  end
endmodule
