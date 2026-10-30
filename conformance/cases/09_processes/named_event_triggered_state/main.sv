// The trigger of a named event is instantaneous, but the event's triggered
// state persists for the rest of the time step in which it was triggered and is
// false again once simulation time advances; the triggered built-in method
// reads that state (LRM 15.5.3).
module Top;
  event signalled;

  bit before_any_trigger;
  bit within_the_step;
  bit after_time_advances;

  initial begin
    before_any_trigger = signalled.triggered;
    #5;
    -> signalled;
    within_the_step = signalled.triggered;
    #5;
    after_time_advances = signalled.triggered;
  end

  final begin
    if (before_any_trigger !== 1'b0)
      $fatal(1, "before_any_trigger was %b, expected 0", before_any_trigger);
    if (within_the_step !== 1'b1)
      $fatal(1, "within_the_step was %b, expected 1", within_the_step);
    if (after_time_advances !== 1'b0)
      $fatal(1, "after_time_advances was %b, expected 0",
             after_time_advances);
    $display("All checks passed");
  end
endmodule
