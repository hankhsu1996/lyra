`timescale 1ns / 1ns
module Test;
  initial begin
    fork
      #10 $display("[%0d] branch A", $time);
      #20 $display("[%0d] branch B", $time);
    join
    $display("[%0d] after join", $time);
  end
endmodule
