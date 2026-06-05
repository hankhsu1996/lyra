`timescale 1ns / 1ns
module Test;
  task run_branches();
    fork
      #10 $display("[%0d] branch A", $time);
      #20 $display("[%0d] branch B", $time);
    join
  endtask

  initial begin
    run_branches();
    $display("[%0d] after task", $time);
  end
endmodule
