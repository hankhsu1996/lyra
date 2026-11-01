// A `disable` of a task an interface declares `extern forkjoin` ends what it
// names (LRM 25.7.4, 9.6.2). Named through a module instance, it ends only that
// module's definition, in every call running it, and every call then waits only
// for the definitions left; named through a module instance defining none, it
// ends nothing; named through the interface instance, it ends every call of the
// task, and each caller resumes past it.
interface Bus;
  extern forkjoin task Ping();
  modport provider(export Ping);
endinterface

module Provider #(
    parameter int DELAY = 0
) (
    Bus.provider b
);
  int finished = 0;

  task b.Ping();
    #DELAY;
    finished = finished + 1;
  endtask
endmodule

module Watcher (
    Bus b
);
endmodule

module Top;
  Bus bus ();
  Provider #(.DELAY(10)) slow (bus.provider);
  Provider #(.DELAY(4)) fast (bus.provider);
  Watcher watcher (bus);

  Bus other ();
  Provider #(.DELAY(10)) held (other.provider);

  int first_back = -1;
  int second_back = -1;
  int third_back = -1;

  // Two calls of one task run at once, so ending one definition has to end it
  // in both.
  initial begin
    bus.Ping();
    first_back = $time;
  end
  initial begin
    #1 bus.Ping();
    second_back = $time;
  end
  initial begin
    #2 disable slow.b.Ping;
    disable watcher.b.Ping;
  end

  // Ended through the interface instance, the call ends with every definition
  // it started.
  initial begin
    #20 other.Ping();
    third_back = $time;
  end
  initial begin
    #23 disable other.Ping;
  end

  final begin
    if (slow.finished !== 0)
      $fatal(1, "slow.finished was %0d, expected 0", slow.finished);
    if (fast.finished !== 2)
      $fatal(1, "fast.finished was %0d, expected 2", fast.finished);
    if (first_back !== 4)
      $fatal(1, "first_back was %0d, expected 4", first_back);
    if (second_back !== 5)
      $fatal(1, "second_back was %0d, expected 5", second_back);
    if (held.finished !== 0)
      $fatal(1, "held.finished was %0d, expected 0", held.finished);
    if (third_back !== 23)
      $fatal(1, "third_back was %0d, expected 23", third_back);
    $display("All checks passed");
  end
endmodule
