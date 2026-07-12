`timescale 1ns / 1ns
module Test;
  // LRM 9.6.1 `wait fork`: block until every immediate child subprocess has
  // terminated, excluding their descendants. `wait fork` runs inside a task,
  // which executes in the enclosing process's thread (LRM 9.5), so it waits on
  // children spawned by the `initial` block before the task was ever called
  // (child1, child2) as well as those the task spawns (child3). The nested
  // `join_none` makes `grandchild` a descendant, not an immediate child, so it
  // is not waited on -- its parent branch terminates the moment it has spawned
  // it, yet its lineage is retained until the grandchild finishes (LRM 9.6.3).
  task automatic do_test();
    fork
      #30 $display("[%0d] child3", $time);
      fork
        #50 $display("[%0d] grandchild", $time);
      join_none
    join_none
    wait fork;
    $display("[%0d] after wait fork", $time);
  endtask

  initial begin
    fork
      #10 $display("[%0d] child1", $time);
      #20 $display("[%0d] child2", $time);
    join_none
    do_test();
    $display("[%0d] initial after do_test", $time);
    #100;
  end
endmodule
