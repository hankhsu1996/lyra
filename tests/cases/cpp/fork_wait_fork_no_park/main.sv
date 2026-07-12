`timescale 1ns / 1ns
module Test;
  // The `wait fork` cases that resolve without suspending (LRM 9.6.1): a process
  // with no live child returns at once. The first `wait fork` runs before any
  // fork, so there is nothing to wait on; the second runs after the spawned
  // child has already finished. Two `wait fork` statements in one process are
  // independent -- each observes the child set as it stands when reached.
  initial begin
    wait fork;
    $display("[%0d] no children", $time);
    fork
      #5 $display("[%0d] child", $time);
    join_none
    #10;
    wait fork;
    $display("[%0d] child already finished", $time);
  end
endmodule
