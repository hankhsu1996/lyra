// A disable statement terminates the activity of the task or named block it
// names, and execution resumes at the statement following that block or
// following the statement that enabled that task. The statements between the
// disable and the end of the target therefore never run, while the statements
// after the target do. A block that is a loop body resumes at the loop
// control, which is what makes disabling it act like continue; disabling an
// enclosing block from inside that body resumes after that block instead,
// past the remainder of it as well as the loop. Disabling a task ends its
// activity from wherever the disable stands, while disabling a block written
// inside a task ends only that block and the rest of the task still runs
// (LRM 9.6.2).
module Top;
  int self_disabled;
  int after_self_disabled;

  int loop_passes;
  int loop_tails;
  int after_loop_inside_outer;
  int after_outer;

  int task_entered;
  int task_after_disable;
  int after_task_call;

  int block_in_task;
  int task_after_block;
  int after_second_call;

  task automatic disables_itself();
    task_entered = 1;
    disable disables_itself;
    task_after_disable = 1;
  endtask

  task automatic disables_its_own_block();
    begin : body
      block_in_task = 1;
      disable body;
      block_in_task = 2;
    end
    task_after_block = 1;
  endtask

  initial begin
    self_disabled = 9;
    block_in_task = 9;

    begin : named
      self_disabled = 1;
      disable named;
      self_disabled = 2;
    end
    after_self_disabled = 1;

    begin : outer
      for (int i = 0; i < 4; i = i + 1) begin : pass
        loop_passes = loop_passes + 1;
        if (i == 1) disable pass;
        if (i == 2) disable outer;
        loop_tails = loop_tails + 1;
      end
      after_loop_inside_outer = 1;
    end
    after_outer = 1;

    disables_itself();
    after_task_call = 1;

    disables_its_own_block();
    after_second_call = 1;
  end

  final begin
    if (self_disabled !== 1)
      $fatal(1, "self_disabled was %0d, expected 1", self_disabled);
    if (after_self_disabled !== 1)
      $fatal(1, "after_self_disabled was %0d, expected 1", after_self_disabled);
    if (loop_passes !== 3)
      $fatal(1, "loop_passes was %0d, expected 3", loop_passes);
    if (loop_tails !== 1)
      $fatal(1, "loop_tails was %0d, expected 1", loop_tails);
    if (after_loop_inside_outer !== 0)
      $fatal(1, "after_loop_inside_outer was %0d, expected 0",
             after_loop_inside_outer);
    if (after_outer !== 1)
      $fatal(1, "after_outer was %0d, expected 1", after_outer);
    if (task_entered !== 1)
      $fatal(1, "task_entered was %0d, expected 1", task_entered);
    if (task_after_disable !== 0)
      $fatal(1, "task_after_disable was %0d, expected 0", task_after_disable);
    if (after_task_call !== 1)
      $fatal(1, "after_task_call was %0d, expected 1", after_task_call);
    if (block_in_task !== 1)
      $fatal(1, "block_in_task was %0d, expected 1", block_in_task);
    if (task_after_block !== 1)
      $fatal(1, "task_after_block was %0d, expected 1", task_after_block);
    if (after_second_call !== 1)
      $fatal(1, "after_second_call was %0d, expected 1", after_second_call);
    $display("All checks passed");
  end
endmodule
