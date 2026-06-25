`timescale 1ns / 1ns
module Test;
  initial begin
    fork
      begin
        automatic int outer_k = 7;
        fork
          #10 $display("inner outer_k=%0d", outer_k);
        join_none
      end
    join_none
    #20;
  end
endmodule
