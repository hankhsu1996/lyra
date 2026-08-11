`timescale 1ns / 1ns
module Test;
  // LRM 9.6.2: disabling a named block terminates not only its own execution but
  // "all activities enabled within" it. An activity is enabled within a block by
  // the execution that was inside the block when it spawned, so what makes it a
  // member is where its spawner was -- not where its own body is written, and
  // not whether it has begun running.
  //
  // Three shapes of that membership, all disabled through the same block:
  //
  // outlives  -- a join_none child that outlives B's body. B has already
  //              finished when the disable arrives at time 10, yet the child is
  //              still an activity enabled within B, so it terminates before its
  //              completion runs.
  // unstarted -- a child disabled before it first runs. A spawned process does
  //              not start until its spawner blocks or terminates (LRM 9.3.2),
  //              so this one is disabled while it exists but has executed
  //              nothing; it must never execute anything.
  // by_call   -- a child spawned by a task called from inside C. Its body is
  //              written in the task, which is nowhere inside C, so only the
  //              spawner's position places it within C.
  int outlives_ran;
  int outlives_done;

  initial begin : B
    fork
      begin
        outlives_ran = 1;
        #100;
        outlives_done = 1;
      end
    join_none
  end

  int unstarted_ran;

  initial begin : U
    fork
      begin
        unstarted_ran = 1;
        #100;
      end
    join_none
    disable U;
  end

  int by_call_done;
  int c_after;

  task automatic spawner();
    fork
      begin
        #100;
        by_call_done = 1;
      end
    join_none
  endtask

  initial begin : C
    spawner();
    #200;
    c_after = 1;
  end

  int disabled_at;

  initial begin
    #10;
    disable B;
    disable C;
    disabled_at = 1;
  end
endmodule
