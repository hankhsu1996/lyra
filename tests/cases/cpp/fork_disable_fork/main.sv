`timescale 1ns / 1ns
module Test;
  // LRM 9.6.3 `disable fork`: terminate every descendant of the calling process
  // -- not only its immediate children, but also the descendants of
  // subprocesses that have already terminated. The caller does not suspend, so
  // the statement after it runs at the same simulation time.
  //
  // The first branch terminates at 10 and satisfies `join_any`, but the
  // grandchild it detached is still parked at that point, and so is the second
  // branch. `disable fork` must reach both, so neither ever prints. Because a
  // task runs in its caller's thread (LRM 9.5), the descendants disabled here
  // are the ones the `initial` process owns.
  task automatic get_first();
    fork
      begin
        #10;
        fork
          #50 $display("[%0d] grandchild", $time);
        join_none
        $display("[%0d] child spawned grandchild", $time);
      end
      #40 $display("[%0d] slow child", $time);
    join_any
    disable fork;
    $display("[%0d] after disable fork", $time);
  endtask

  initial begin
    // No process has been spawned yet, so this reaches an empty descendant set.
    disable fork;
    get_first();
    $display("[%0d] initial after get_first", $time);
    #100;
    $display("[%0d] done", $time);
  end
endmodule
