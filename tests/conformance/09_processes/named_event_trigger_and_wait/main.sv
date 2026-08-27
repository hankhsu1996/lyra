// A named event is made to occur by the -> operator and is waited for with the
// event control operator, which blocks the calling procedure until the event is
// triggered (LRM 15.5.1, 15.5.2). One trigger unblocks every procedure that is
// waiting on that event at the time and no others, so several waiters on one
// event all resume together while two named events stay two independent
// synchronization objects. The event a procedure names may be declared in a
// scope enclosing it.
module Top;
  event plain;
  event early, late;
  event broadcast;
  event outer;

  time plain_at;
  time early_at;
  time late_at;
  time first_listener_at;
  time second_listener_at;
  time third_listener_at;
  time outer_at;

  initial begin
    @plain;
    plain_at = $time;
  end

  initial begin
    @early;
    early_at = $time;
  end

  initial begin
    @late;
    late_at = $time;
  end

  initial begin
    @broadcast;
    first_listener_at = $time;
  end

  initial begin
    @broadcast;
    second_listener_at = $time;
  end

  initial begin
    @broadcast;
    third_listener_at = $time;
  end

  if (1) begin : g
    initial begin
      @outer;
      outer_at = $time;
    end
  end

  initial begin
    #5;
    -> plain;
    -> broadcast;
    -> early;
    -> outer;
    #5;
    -> late;
  end

  final begin
    if (plain_at !== 5) $fatal(1, "plain_at was %0d, expected 5", plain_at);
    if (early_at !== 5) $fatal(1, "early_at was %0d, expected 5", early_at);
    if (late_at !== 10) $fatal(1, "late_at was %0d, expected 10", late_at);
    if (first_listener_at !== 5)
      $fatal(1, "first_listener_at was %0d, expected 5", first_listener_at);
    if (second_listener_at !== 5)
      $fatal(1, "second_listener_at was %0d, expected 5", second_listener_at);
    if (third_listener_at !== 5)
      $fatal(1, "third_listener_at was %0d, expected 5", third_listener_at);
    if (outer_at !== 5) $fatal(1, "outer_at was %0d, expected 5", outer_at);
    $display("All checks passed");
  end
endmodule
