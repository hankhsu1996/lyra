// A nonblocking event trigger executes without blocking, and creates a
// nonblocking assign update event in the time in which its delay control
// expires or its event control occurs; the effect of that update is to trigger
// the event in the nonblocking assignment region of the simulation cycle
// (LRM 15.5.1, 4.4.2.4). The standard makes no process of such an update, so
// nothing that names processes waits for one (LRM 9.6.1).
module Top;
  event plain;
  event delayed;
  event on_edge;
  event counted;
  event unlineaged;

  bit clk = 0;

  time plain_at;
  time delayed_at;
  time carried_on_at;
  time on_edge_at;
  time counted_at;
  time unlineaged_at;
  time wait_fork_returned_at;

  initial repeat (8) #5 clk = ~clk;

  // The three placements below -- no control, a delay, an event -- are each
  // watched by a procedure that only reaches the wait in the inactive region of
  // the slot the trigger is due in. Such a procedure is in time for a trigger
  // in that slot's nonblocking assignment region and misses one made where the
  // statement stands, so each of those checks reads both which slot the trigger
  // landed in and which region of it.
  //
  // With no control the slot is the one the statement stands in.
  initial begin
    #20;
    ->> plain;
  end

  initial begin
    #20;
    #0;
    @plain;
    plain_at = $time;
  end

  // A delay control names the slot the update is due in, and the procedure
  // that wrote it carries on where it stands.
  initial begin
    #20;
    ->> #7 delayed;
    carried_on_at = $time;
  end

  initial begin
    #27;
    #0;
    @delayed;
    delayed_at = $time;
  end

  // An event control names that slot instead, however far off it is.
  initial begin
    #22;
    ->> @(posedge clk) on_edge;
  end

  initial begin
    #25;
    #0;
    @on_edge;
    on_edge_at = $time;
  end

  // A repeat event control waits out that many occurrences of the event.
  initial begin
    #22;
    ->> repeat (2) @(posedge clk) counted;
  end

  initial begin
    @counted;
    counted_at = $time;
  end

  // An update still to come is not a subprocess of the procedure that wrote
  // it, so `wait fork` returns without waiting for the trigger it carries.
  initial begin
    ->> @(posedge clk) unlineaged;
    wait fork;
    wait_fork_returned_at = $time;
  end

  initial begin
    @unlineaged;
    unlineaged_at = $time;
  end

  final begin
    if (plain_at !== 20)
      $fatal(1, "the uncontrolled trigger woke a wait at %0d, expected 20",
             plain_at);
    if (carried_on_at !== 20)
      $fatal(1, "the trigger's own procedure carried on at %0d, expected 20",
             carried_on_at);
    if (delayed_at !== 27)
      $fatal(1, "the delayed trigger woke a wait at %0d, expected 27",
             delayed_at);
    if (on_edge_at !== 25)
      $fatal(1, "the edge-controlled trigger woke a wait at %0d, expected 25",
             on_edge_at);
    if (counted_at !== 35)
      $fatal(1, "the repeated trigger woke a wait at %0d, expected 35",
             counted_at);
    if (unlineaged_at !== 5)
      $fatal(1, "the unlineaged trigger woke a wait at %0d, expected 5",
             unlineaged_at);
    if (wait_fork_returned_at !== 0)
      $fatal(1, "wait fork returned at %0d, expected 0",
             wait_fork_returned_at);
    $display("All checks passed");
  end
endmodule
