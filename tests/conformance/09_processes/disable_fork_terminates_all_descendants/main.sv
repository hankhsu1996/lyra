// disable fork terminates every descendant subprocess of the calling process,
// not only its immediate children, and reaches the descendants of subprocesses
// that have already terminated (LRM 9.6.3). The calling process is not blocked
// by it, so the statement after it runs in the same time step, and a process
// with no descendant at all is not blocked either. The lineage it considers is
// the dynamic parent-child one, and a task enable does not start a thread of
// its own (LRM 9.5), so a disable fork written in a task body also reaches
// what the process that enabled the task had spawned before the call.
module Top;
  int child_ran, grandchild_ran, sibling_ran, outer_child_ran;
  int resume_time, after_disable;
  int reached_after_empty_disable, after_task_time;

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
    $display("All checks passed");
  end
endmodule
