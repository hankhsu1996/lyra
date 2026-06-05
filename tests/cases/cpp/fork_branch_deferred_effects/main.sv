`timescale 1ns / 1ns
module Test;
  int a;
  initial begin
    fork
      begin
        a <= 5;
        $strobe("[%0d] strobe a=%0d", $time, a);
      end
      #10 a = 9;
    join
    $display("[%0d] after join a=%0d", $time, a);
  end
endmodule
