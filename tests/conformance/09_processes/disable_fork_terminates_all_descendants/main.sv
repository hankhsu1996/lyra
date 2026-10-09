// disable fork terminates every descendant subprocess of the calling process,
// not only its immediate children, and reaches the descendants of subprocesses
// that have already terminated (LRM 9.6.3). The calling process is not blocked
// by it, so the statement after it runs in the same time step, and a process
// with no descendant at all is not blocked either. The lineage it considers is
// the dynamic parent-child one, and a task enable does not start a thread of
// its own (LRM 9.5), so a disable fork written in a task body also reaches
// what the process that enabled the task had spawned before the call.
//
// What it terminates goes as a whole, whatever its parts were waiting on --
// one descendant awaiting another included (LRM 9.7) -- and what goes with it
// is only the descendants: a descendant inside the same named block as the
// process that disabled it leaves that block, and the process stays inside, so
// a later `disable` of the block still reaches it (LRM 9.6.2).
module Top;
  int child_ran, grandchild_ran, sibling_ran, outer_child_ran;
  int resume_time, after_disable;
  int reached_after_empty_disable, after_task_time;
  int awaiting_resumed;
  int spawner_left_region;
  process awaited;

  task automatic take_first();
    fork
      begin
        #10;
        fork
          #50 grandchild_ran = 1;
        join_none
        child_ran = 1;
      end
      #40 sibling_ran = 1;
    join_any
    disable fork;
    resume_time = $time;
    after_disable = 1;
  endtask

  initial begin
    disable fork;
    reached_after_empty_disable = 1;
    fork
      #60 outer_child_ran = 1;
    join_none
    take_first();
    after_task_time = $time;
    #100;
  end

  initial begin
    fork
      begin
        fork
          begin
            awaited = process::self();
            #100;
          end
        join_none
        #0 awaited.await();
        awaiting_resumed = 1;
      end
    join_none
    #1 disable fork;
  end

  task automatic stay_in_region(input bit spawn);
    begin : region
      if (spawn) begin
        fork
          stay_in_region(0);
        join_none
        #1 disable fork;
      end
      #5;
      if (spawn) spawner_left_region = 0;
    end
  endtask

  initial begin
    spawner_left_region = 1;
    stay_in_region(1);
  end

  initial #2 disable stay_in_region.region;

  final begin
    if (reached_after_empty_disable !== 1)
      $fatal(1, "reached_after_empty_disable was %0d, expected 1",
             reached_after_empty_disable);
    if (child_ran !== 1)
      $fatal(1, "child_ran was %0d, expected 1", child_ran);
    if (grandchild_ran !== 0)
      $fatal(1, "grandchild_ran was %0d, expected 0", grandchild_ran);
    if (sibling_ran !== 0)
      $fatal(1, "sibling_ran was %0d, expected 0", sibling_ran);
    if (outer_child_ran !== 0)
      $fatal(1, "outer_child_ran was %0d, expected 0", outer_child_ran);
    if (after_disable !== 1)
      $fatal(1, "after_disable was %0d, expected 1", after_disable);
    if (resume_time !== 10)
      $fatal(1, "resume_time was %0d, expected 10", resume_time);
    if (after_task_time !== 10)
      $fatal(1, "after_task_time was %0d, expected 10", after_task_time);
    if (awaiting_resumed !== 0)
      $fatal(1, "a descendant awaiting another resumed after both were disabled");
    if (spawner_left_region !== 1)
      $fatal(1, "a process left a block its disabled descendant was inside");
    $display("All checks passed");
  end
endmodule
