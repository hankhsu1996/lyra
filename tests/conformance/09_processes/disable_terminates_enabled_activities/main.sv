// A disable statement terminates the activity of the named block or task it
// names and every activity enabled within it, so a task the block had enabled
// and a process the block had forked end where they stand instead of resuming
// later, and a task enabled more than once loses every activation. A disable
// reaching a block from an unrelated process is allowed, without regard to any
// parent-child relationship (LRM 9.6, 9.6.2). What makes a forked process one
// of the activities enabled within a block is that the block spawned it rather
// than that it has begun running: such a process does not start until its
// spawner blocks or terminates (LRM 9.3.2), so a block that disables itself
// straight after spawning one ends it before it has executed anything.
module Top;
  int parked_entered;
  int parked_after_delay;

  int called_task_entered;
  int called_task_after_delay;
  int caller_after_call;

  int forked_child_started;
  int forked_child_finished;
  int spawner_after_delay;

  int unstarted_child_ran;

  int activations_entered;
  int activations_completed;

  int disable_time;
  int simulation_outlived_the_delays;

  task automatic called_by_a_block();
    called_task_entered = 1;
    #100;
    called_task_after_delay = 1;
  endtask

  task automatic enabled_twice();
    activations_entered = activations_entered + 1;
    #100;
    activations_completed = activations_completed + 1;
  endtask

  initial begin : parked
    parked_entered = 1;
    #100;
    parked_after_delay = 1;
  end

  initial begin : caller
    called_by_a_block();
    caller_after_call = 1;
  end

  initial begin : spawner
    fork
      begin
        forked_child_started = 1;
        #100;
        forked_child_finished = 1;
      end
    join_none
    #100;
    spawner_after_delay = 1;
  end

  initial begin : spawns_then_disables_itself
    fork
      unstarted_child_ran = 1;
    join_none
    disable spawns_then_disables_itself;
  end

  initial fork
    enabled_twice();
    enabled_twice();
  join_none

  initial begin
    #10;
    disable parked;
    disable caller;
    disable spawner;
    disable enabled_twice;
    disable_time = $time;
  end

  initial begin
    #200;
    simulation_outlived_the_delays = 1;
  end

  final begin
    if (disable_time !== 10)
      $fatal(1, "the disabling process resumed at %0d, expected 10",
             disable_time);
    if (simulation_outlived_the_delays !== 1)
      $fatal(1, "simulation ended before the disabled delays would have run");
    if (parked_entered !== 1)
      $fatal(1, "parked_entered was %0d, expected 1", parked_entered);
    if (parked_after_delay !== 0)
      $fatal(1, "parked_after_delay was %0d, expected 0", parked_after_delay);
    if (called_task_entered !== 1)
      $fatal(1, "called_task_entered was %0d, expected 1", called_task_entered);
    if (called_task_after_delay !== 0)
      $fatal(1, "called_task_after_delay was %0d, expected 0",
             called_task_after_delay);
    if (caller_after_call !== 0)
      $fatal(1, "caller_after_call was %0d, expected 0", caller_after_call);
    if (forked_child_started !== 1)
      $fatal(1, "forked_child_started was %0d, expected 1",
             forked_child_started);
    if (forked_child_finished !== 0)
      $fatal(1, "forked_child_finished was %0d, expected 0",
             forked_child_finished);
    if (spawner_after_delay !== 0)
      $fatal(1, "spawner_after_delay was %0d, expected 0", spawner_after_delay);
    if (unstarted_child_ran !== 0)
      $fatal(1, "unstarted_child_ran was %0d, expected 0", unstarted_child_ran);
    if (activations_entered !== 2)
      $fatal(1, "activations_entered was %0d, expected 2", activations_entered);
    if (activations_completed !== 0)
      $fatal(1, "activations_completed was %0d, expected 0",
             activations_completed);
    $display("All checks passed");
  end
endmodule
