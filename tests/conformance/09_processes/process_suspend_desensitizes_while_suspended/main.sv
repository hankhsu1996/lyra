// suspend() stops a process, and status() then reports SUSPENDED. A process
// suspended while waiting in a blocking statement is desensitized to whatever
// it is blocked on, so it does not advance while suspended and an event that
// occurs meanwhile does not reach it. resume() resensitizes it to the same
// thing, and the standard names three of them: an event expression, so that
// only a later occurrence wakes it; a delay, which it goes on waiting for and
// which, where it has already transpired, continues the process in the current
// time step; and a wait condition, which becomes true or does not, and where it
// has become true meanwhile continues the process in the current time step
// (LRM 9.7).
//
// A `disable` of a block the suspended process is inside still ends that
// block for it (LRM 9.6.2): resumed, it leaves the block at once rather than
// waiting on, in the current time step.
module Top;
  int delayed_suspended, delayed_not_progressed;
  int delayed_marker, delayed_ran_time, delayed_after_resume;

  int event_suspended, wake_count, last_wake_time;

  int pending_ran_time, condition_ran_time;

  int held_ran_on, held_left_time;

  bit sig;
  bit gate;
  event never_fires;

  process delayed;
  process watcher;
  process pending;
  process conditioned;
  process held;

  task automatic hold();
    begin : held_block
      @(never_fires);
      held_ran_on = 1;
    end
    held_left_time = $time;
  endtask

  initial begin
    held_left_time = -1;
    fork
      begin
        held = process::self();
        hold();
      end
    join_none
    #1 held.suspend();
    disable hold.held_block;
    #1 held.resume();
  end

  initial begin
    fork
      begin
        delayed = process::self();
        #50;
        delayed_marker = 7;
        delayed_ran_time = $time;
      end
    join_none

    #1;
    delayed.suspend();
    delayed_suspended = (delayed.status() == process::SUSPENDED);

    #100;
    delayed_not_progressed = (delayed_marker == 0);
    delayed.resume();

    #1;
    delayed_after_resume = delayed_marker;
  end

  initial begin
    fork
      begin
        watcher = process::self();
        forever begin
          @(posedge sig);
          wake_count = wake_count + 1;
          last_wake_time = $time;
        end
      end
    join_none

    #1;
    watcher.suspend();
    event_suspended = (watcher.status() == process::SUSPENDED);

    #1 sig = 1;
    #1 sig = 0;
    watcher.resume();

    #1 sig = 1;
    #1;
  end

  initial begin
    fork
      begin
        pending = process::self();
        #50;
        pending_ran_time = $time;
      end
      begin
        conditioned = process::self();
        wait (gate == 1);
        condition_ran_time = $time;
      end
    join_none

    #1;
    pending.suspend();
    conditioned.suspend();

    #4 gate = 1;

    #15;
    pending.resume();
    conditioned.resume();
  end

  final begin
    if (delayed_suspended !== 1)
      $fatal(1, "delayed_suspended was %0d, expected 1", delayed_suspended);
    if (delayed_not_progressed !== 1)
      $fatal(1, "delayed_not_progressed was %0d, expected 1",
             delayed_not_progressed);
    if (delayed_ran_time !== 101)
      $fatal(1, "delayed_ran_time was %0d, expected 101", delayed_ran_time);
    if (delayed_after_resume !== 7)
      $fatal(1, "delayed_after_resume was %0d, expected 7",
             delayed_after_resume);
    if (event_suspended !== 1)
      $fatal(1, "event_suspended was %0d, expected 1", event_suspended);
    if (wake_count !== 1)
      $fatal(1, "wake_count was %0d, expected 1", wake_count);
    if (last_wake_time !== 4)
      $fatal(1, "last_wake_time was %0d, expected 4", last_wake_time);
    if (pending_ran_time !== 50)
      $fatal(1, "pending_ran_time was %0d, expected 50", pending_ran_time);
    if (condition_ran_time !== 20)
      $fatal(1, "condition_ran_time was %0d, expected 20", condition_ran_time);
    if (held_ran_on !== 0 || held_left_time !== 2)
      $fatal(1, "a block disabled while suspended was left at %0d, expected 2",
             held_left_time);
    $display("All checks passed");
  end
endmodule
