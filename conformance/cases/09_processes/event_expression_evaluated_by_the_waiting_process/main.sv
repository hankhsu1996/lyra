// A change to what an event expression reads schedules an evaluation of the
// waiting process, which evaluates the expression once and compares (LRM 4.5:
// an update event schedules "evaluation event for any process sensitive to the
// object"; LRM 9.4.2: a change to a member a method reads reevaluates the
// expression). So a call the expression makes runs once where the wait begins
// and once per reevaluation, and an `iff` qualifier runs once each time the
// watched expression moves (LRM 9.4.2.3).
//
// The evaluation is the waiting process's, so a call in it runs as that
// process: `process::self()` there answers the waiter, not whoever wrote what
// the expression reads (LRM 9.7). And what it writes is written by a process
// that is evaluating, not waiting, so it is no event for that process: a call
// that changes what its own wait reads -- a property of the object a method
// waits on, a variable the expression watches, the named event the wait is
// for -- leaves the wait waiting for the next change made by someone else.
interface Bus;
  logic grant = 1'b0;
endinterface

class Waiter;
  virtual Bus buses[2];
  int calls;
  time woke = 0;

  function automatic int counted(int v);
    calls++;
    return v;
  endfunction

  task automatic Await();
    @(posedge buses[counted(0)].grant);
    woke = $time;
  endtask
endclass

module Top;
  int x;
  int sum_calls;
  int held_calls;
  int gate_calls;
  int trigger_calls;
  logic gated = 1'b0;
  event ping;
  Bus first ();
  Bus second ();
  virtual Bus held[2];
  Waiter waiter;

  time sum_woke = 0;
  time held_woke = 0;
  time gated_woke = 0;
  time ping_woke = 0;

  int sum_calls_armed = -1;
  int held_calls_armed = -1;
  int method_calls_armed = -1;

  int y;
  process asker;
  process waiter_self;
  bit asked_by_the_waiter = 1'b0;
  time asked_woke = 0;

  // Records which process evaluates it, after the wait began.
  function automatic int ask(int v);
    if (asker != null) asked_by_the_waiter = process::self() == waiter_self;
    asker = process::self();
    return v;
  endfunction

  function automatic int count_sum(int v);
    sum_calls++;
    return v;
  endfunction

  function automatic int count_held(int v);
    held_calls++;
    return v;
  endfunction

  // Toggles what its own wait watches, and holds the event back.
  function automatic bit toggle_gated();
    gate_calls++;
    gated = ~gated;
    return 1'b0;
  endfunction

  // Triggers the event its own wait is for, and holds the event back.
  function automatic bit trigger_ping();
    trigger_calls++;
    ->ping;
    return 1'b0;
  endfunction

  initial begin
    waiter = new;
    waiter.buses[0] = first;
    held[0] = second;
    fork
      begin @(x + count_sum(0)); sum_woke = $time; end
      begin @(posedge held[count_held(0)].grant); held_woke = $time; end
      begin @(gated iff toggle_gated()); gated_woke = $time; end
      begin @(ping iff trigger_ping()); ping_woke = $time; end
      begin
        waiter_self = process::self();
        @(y + ask(0));
        asked_woke = $time;
      end
      waiter.Await();
    join_none
    #1;
    sum_calls_armed = sum_calls;
    held_calls_armed = held_calls;
    method_calls_armed = waiter.calls;
    #1 x = 1;
    #1 second.grant = 1'b1;
    #1 first.grant = 1'b1;
    #1 gated = 1'b1;
    #1 ->ping;
    #1 y = 1;
  end

  final begin
    if (sum_calls_armed !== 1)
      $fatal(1, "a call in the expression ran %0d times where the wait began, expected once", sum_calls_armed);
    if (sum_woke !== 2 || sum_calls !== 2)
      $fatal(1, "the sum woke at %0t having run its call %0d times, expected at 2 after 2", sum_woke, sum_calls);
    if (held_calls_armed !== 1)
      $fatal(1, "a call choosing a virtual interface ran %0d times where the wait began, expected once", held_calls_armed);
    if (held_woke !== 3 || held_calls !== 2)
      $fatal(1, "the interface wait woke at %0t having run its call %0d times, expected at 3 after 2", held_woke, held_calls);
    if (method_calls_armed !== 1)
      $fatal(1, "a method's call writing its own object ran %0d times where the wait began, expected once", method_calls_armed);
    if (waiter.woke !== 4 || waiter.calls !== 2)
      $fatal(1, "the method's wait woke at %0t having run its call %0d times, expected at 4 after 2", waiter.woke, waiter.calls);
    if (gated_woke !== 0 || gate_calls !== 1)
      $fatal(1, "a qualifier toggling what it gates ran %0d times and woke at %0t, expected once and never", gate_calls, gated_woke);
    if (ping_woke !== 0 || trigger_calls !== 1)
      $fatal(1, "a qualifier triggering its own event ran %0d times and woke at %0t, expected once and never", trigger_calls, ping_woke);
    if (asked_woke !== 7 || !asked_by_the_waiter)
      $fatal(1, "the wait asking which process evaluates it woke at %0t, the asker %s the waiter, expected at 7 and the waiter", asked_woke, asked_by_the_waiter ? "was" : "was not");
    $display("All checks passed");
  end
endmodule
