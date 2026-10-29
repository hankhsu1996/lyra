// A non-blocking assignment carrying an intra-assignment delay reads its
// right-hand side where the statement is reached and schedules the update into
// the NBA region of the slot that delay names, without suspending the procedure
// (LRM 9.4.5, 4.4.2.4, 10.4.2). Several such updates to one variable stay live
// at once, and each lands at its own time. The right-hand side is evaluated
// before the delay elapses; whether before or after the delay expression itself
// is not settled, so nothing here depends on that order.
module Top;
  int source = 1;
  int target = 0;
  int seen_before_deadline = 9;
  int stepped = 9;
  int stepped_at_four = 9;
  int stepped_at_eight = 9;

  initial begin
    target <= #10 source;
    // The procedure did not block, so it is still at time zero here and the
    // update has not been made.
    seen_before_deadline = target;
  end

  // LRM 10.4.2: scheduling one update does not cancel another already pending
  // on the same variable, so all three land, each at its own time.
  initial begin
    stepped <= #4 1;
    stepped <= #8 2;
    stepped <= #12 3;
  end

  initial begin
    #6 stepped_at_four = stepped;
    #4 stepped_at_eight = stepped;
  end

  // LRM 4.6: updates are performed in the order they were placed, so one placed
  // at time zero for time 20 lands before one placed at time 20 itself.
  int ordered = 0;
  initial begin
    ordered <= #20 1;
    #20 ordered <= 2;
  end

  // Rewriting the operand while the delays are under way reaches no update,
  // because every right-hand side was read at time zero.
  initial #5 source = 99;

  // The right-hand side is read before the delay elapses, whatever amount the
  // delay expression gives.
  int read_first = 0;
  function automatic int right_side();
    return 5;
  endfunction
  initial read_first <= #3 right_side();

  final begin
    if (seen_before_deadline !== 0)
      $fatal(
          1, "the procedure read %0d without blocking, expected the un-updated 0",
          seen_before_deadline);
    if (target !== 1)
      $fatal(1, "the scheduled update stored %0d, expected the operand read at time zero", target);
    if (stepped_at_four !== 1)
      $fatal(1, "at time 6 the variable held %0d, expected 1", stepped_at_four);
    if (stepped_at_eight !== 2)
      $fatal(1, "at time 10 the variable held %0d, expected 2", stepped_at_eight);
    if (stepped !== 3)
      $fatal(1, "the last update left %0d, expected 3", stepped);
    if (ordered !== 2)
      $fatal(1, "an update placed earlier for one slot landed after one placed in it: %0d", ordered);
    if (read_first !== 5)
      $fatal(1, "the delayed update stored %0d, expected 5", read_first);
    $display("All checks passed");
  end
endmodule
