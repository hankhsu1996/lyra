`timescale 1ns / 1ns
module Test;
  // A task body is a coroutine, so a fork inside it may await its join (LRM
  // 9.3.2). A branch reads the task's automatic local by reference, and the
  // join holds the task -- and so its caller -- until the slowest branch ends.
  task automatic run_branches();
    automatic int x = 5;
    fork
      #10 $display("[%0d] branch A x=%0d", $time, x);
      #20 $display("[%0d] branch B", $time);
    join
  endtask

  initial begin
    run_branches();
    $display("[%0d] after task", $time);
  end
endmodule
