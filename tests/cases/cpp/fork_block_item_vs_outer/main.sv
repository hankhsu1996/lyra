`timescale 1ns / 1ns
module Test;
  initial begin
    static int x = 1;
    fork
      automatic int k = x;
      begin
        automatic int m = x + 1;
        x = 9;
        $display("k=%0d m=%0d x=%0d", k, m, x);
      end
    join
  end
endmodule
