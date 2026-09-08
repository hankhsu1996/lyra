// A named event's triggered state persists for the rest of the time step it was
// triggered in, and no longer (LRM 15.5.3), so a procedure waiting on that state
// unblocks whether it reached the wait before the trigger or only in the same
// time step as the trigger -- which is not so of a procedure waiting on the
// event itself -- and a procedure reaching the wait in a later time step stays
// blocked.
module Top;
  event signalled;
  event same_step;

  time waited_from_time_zero_at;
  time waited_in_same_step_at;
  bit lapsed_wait_reached = 0;
  bit lapsed_wait_unblocked = 0;

  initial begin
    wait (signalled.triggered);
    waited_from_time_zero_at = $time;
  end

  initial begin
    #5;
    -> signalled;
  end

  initial begin
    #5;
    -> same_step;
  end

  initial begin
    #5;
    wait (same_step.triggered);
    waited_in_same_step_at = $time;
  end

  initial begin
    #9;
    lapsed_wait_reached = 1;
    wait (signalled.triggered);
    lapsed_wait_unblocked = 1;
  end

  final begin
    if (waited_from_time_zero_at !== 5)
      $fatal(1, "waited_from_time_zero_at was %0d, expected 5",
             waited_from_time_zero_at);
    if (waited_in_same_step_at !== 5)
      $fatal(1, "waited_in_same_step_at was %0d, expected 5",
             waited_in_same_step_at);
    if (lapsed_wait_reached !== 1)
      $fatal(1, "the wait on a lapsed triggered state was never reached");
    if (lapsed_wait_unblocked !== 0)
      $fatal(1, "a triggered-state wait unblocked a later time step");
    $display("All checks passed");
  end
endmodule
