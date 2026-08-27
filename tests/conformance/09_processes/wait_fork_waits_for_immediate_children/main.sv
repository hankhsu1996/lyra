// wait fork blocks a process until every one of its immediate child
// subprocesses -- the processes it created, excluding their descendants -- has
// terminated (LRM 9.6.1). A task enable does not start a thread of its own
// (LRM 9.5), so a wait fork written in a task body waits on the children the
// enabling process spawned before the call as well as on those the task
// spawned itself. What it does not wait on is a grandchild, and a process
// whose immediate children have all terminated is not blocked by it at all.
module Top;
  int child1_time, child2_time, child3_time, grandchild_time;
  int first_wait_reached;
  int task_wait_time, grandchild_done_at_task_wait;
  int second_wait_time, grandchild_done_at_second_wait;

  task automatic spawn_and_wait();
    fork
      #30 child3_time = $time;
      fork
        #50 grandchild_time = $time;
      join_none
    join_none
    wait fork;
    task_wait_time = $time;
    grandchild_done_at_task_wait = (grandchild_time != 0);
  endtask

  initial begin
    wait fork;
    first_wait_reached = 1;

    fork
      #10 child1_time = $time;
      #20 child2_time = $time;
    join_none

    spawn_and_wait();

    wait fork;
    second_wait_time = $time;
    grandchild_done_at_second_wait = (grandchild_time != 0);

    #60;
  end

  final begin
    if (first_wait_reached !== 1)
      $fatal(1, "first_wait_reached was %0d, expected 1", first_wait_reached);
    if (child1_time !== 10)
      $fatal(1, "child1_time was %0d, expected 10", child1_time);
    if (child2_time !== 20)
      $fatal(1, "child2_time was %0d, expected 20", child2_time);
    if (child3_time !== 30)
      $fatal(1, "child3_time was %0d, expected 30", child3_time);
    if (task_wait_time !== 30)
      $fatal(1, "task_wait_time was %0d, expected 30", task_wait_time);
    if (grandchild_done_at_task_wait !== 0)
      $fatal(1, "grandchild_done_at_task_wait was %0d, expected 0",
             grandchild_done_at_task_wait);
    if (second_wait_time !== 30)
      $fatal(1, "second_wait_time was %0d, expected 30", second_wait_time);
    if (grandchild_done_at_second_wait !== 0)
      $fatal(1, "grandchild_done_at_second_wait was %0d, expected 0",
             grandchild_done_at_second_wait);
    if (grandchild_time !== 50)
      $fatal(1, "grandchild_time was %0d, expected 50", grandchild_time);
    $display("All checks passed");
  end
endmodule
