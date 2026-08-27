// A task enable is not among the constructs for which a thread of execution is
// created (LRM 9.5), so a task body runs in the process that enabled it. A
// fork-join block in that body therefore blocks the caller: it does not reach
// the statement after the call until the last of the parallel statements has
// terminated (LRM 9.3.2, Table 9-1). The branches read the task's own
// automatic locals while the frame is still held open by the join.
module Top;
  int branch_a_time, branch_b_time;
  int after_task_time;
  int task_local_seen;

  task automatic run_branches();
    automatic int task_local = 5;
    fork
      #10 branch_a_time = $time;
      begin
        #20;
        branch_b_time = $time;
        task_local_seen = task_local;
      end
    join
  endtask

  initial begin
    run_branches();
    after_task_time = $time;
    #5;
  end

  final begin
    if (branch_a_time !== 10)
      $fatal(1, "branch_a_time was %0d, expected 10", branch_a_time);
    if (branch_b_time !== 20)
      $fatal(1, "branch_b_time was %0d, expected 20", branch_b_time);
    if (after_task_time !== 20)
      $fatal(1, "after_task_time was %0d, expected 20", after_task_time);
    if (task_local_seen !== 5)
      $fatal(1, "task_local_seen was %0d, expected 5", task_local_seen);
    $display("All checks passed");
  end
endmodule
