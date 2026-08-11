`timescale 1ns / 1ns
module Test;
  // LRM 9.7 `status()` reports KILLED for a process forcibly terminated via
  // `disable` -- whether by `disable fork` (LRM 9.6.3) reaching it as a
  // descendant, or by disabling the named block it was enabled within (LRM
  // 9.6.2). A child that instead runs to the end of its body reports FINISHED.
  // The distinction is read through a handle that outlives the child.
  int fork_killed_seen;
  int block_killed_seen;
  int finished_seen;

  process fork_child;
  process block_child;
  process normal_child;

  initial begin
    fork
      begin
        fork_child = process::self();
        #100;
      end
    join_none

    #1;
    disable fork;
    fork_killed_seen = (fork_child.status() == process::KILLED);
  end

  initial begin : B
    fork
      begin
        block_child = process::self();
        #100;
      end
    join_none
    fork
      begin
        normal_child = process::self();
        #5;
      end
    join_none
  end

  initial begin
    #10;
    disable B;
    #20;
    block_killed_seen = (block_child.status() == process::KILLED);
    finished_seen = (normal_child.status() == process::FINISHED);
  end
endmodule
