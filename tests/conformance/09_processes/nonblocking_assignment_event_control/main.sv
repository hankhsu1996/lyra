// A nonblocking assignment carrying an event control reads its right-hand side
// where the statement is reached and leaves the update for the event, without
// suspending the procedure (LRM 9.4.5, 10.4.2). Everything the update needs is
// settled where the statement stands -- the value it writes, and the place it
// writes it, index and all -- so a later write to any of that does not reach an
// update still to come, and two updates to one variable land in the order the
// statements ran.
//
// What is not settled there is which time slot the update lands in: that is the
// slot the event happens in, and within it the update is a nonblocking update
// like any other, so a procedure the same event wakes reads the value the
// target held before it. The repeat form waits for that many occurrences of the
// event first. The standard makes no process of any of this, so `wait fork`
// does not wait for an update that has not happened yet.
module Top;
  logic clk = 0;
  event tick;
  int source = 1;

  int on_edge = 9;
  int on_event = 9;
  int after_two = 9;
  int ordered = 9;
  int mem[0:1];
  int index = 0;
  int read_before_edge = 9;
  int wait_fork_time = 9;
  int seen_at_edge = 9;

  // Rising edges at 5, 15, 25 and 35.
  initial repeat (8) #5 clk = ~clk;
  initial #7 -> tick;

  // Rewriting the operand after time zero reaches none of the updates.
  initial #1 source = 99;

  // The event that makes an update due wakes this procedure in the same slot,
  // and the update is not made until the nonblocking region of it.
  initial begin
    @(posedge clk);
    seen_at_edge = on_edge;
  end

  initial begin
    mem[0] = 9;
    mem[1] = 9;
    on_edge <= @(posedge clk) source;
    on_event <= @(tick) source;
    after_two <= repeat (2) @(posedge clk) source;
    ordered <= @(posedge clk) 1;
    ordered <= @(posedge clk) 2;
    mem[index] <= @(posedge clk) source;
    index = 1;
    // Nothing above blocked, so the procedure is still at time zero and no
    // update has been made -- and `wait fork` has nothing to wait for.
    read_before_edge = on_edge;
    wait fork;
    wait_fork_time = $time;
  end

  final begin
    if (read_before_edge !== 9)
      $fatal(
          1, "the procedure read %0d without blocking, expected the un-updated 9",
          read_before_edge);
    if (wait_fork_time !== 0)
      $fatal(
          1, "`wait fork` returned at time %0d, expected 0 -- a pending update is not a process",
          wait_fork_time);
    if (seen_at_edge !== 9)
      $fatal(
          1, "a procedure woken by the same edge read %0d, expected the un-updated 9",
          seen_at_edge);
    if (on_edge !== 1)
      $fatal(1, "the edge-scheduled update stored %0d, expected 1", on_edge);
    if (on_event !== 1)
      $fatal(1, "the named-event-scheduled update stored %0d, expected 1", on_event);
    if (after_two !== 1)
      $fatal(1, "the two-edge-scheduled update stored %0d, expected 1", after_two);
    if (ordered !== 2)
      $fatal(1, "two updates to one variable left %0d, expected the later 2", ordered);
    if (mem[0] !== 1)
      $fatal(1, "element 0 holds %0d, expected the update to reach the index the statement read",
             mem[0]);
    if (mem[1] !== 9)
      $fatal(1, "the update reached element 1, whose index was written after the statement");
    $display("All checks passed");
  end
endmodule
