`timescale 1ns / 1ns
module Test;
  task automatic run_branches();
    automatic int x = 5;
    fork
      #10 $display("%0d", x);
    join
  endtask

  initial run_branches();
endmodule
