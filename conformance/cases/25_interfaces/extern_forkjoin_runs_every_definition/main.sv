// A task an interface declares `extern forkjoin` may be defined by every module
// connected to the instance (LRM 25.7.4). A call runs every definition at once,
// as a `fork ... join` of them would, and returns once the last has finished,
// so definitions that each wait a different time all start at the call and the
// caller resumes at the longest wait. An argument reaches every definition, and
// each definition hands back its `output` and `inout` formals as it ends, so the
// last to end is what the caller finds.
interface Bus;
  int arrivals = 0;
  extern forkjoin task Ping(input int tag);
  extern forkjoin task Sum(input int tag, output int total, inout int count);
  modport provider(ref arrivals, export Ping, export Sum);
endinterface

module Provider #(
    parameter int DELAY = 0
) (
    Bus.provider b
);
  int started = -1;
  int finished = -1;
  int got = 0;

  task b.Ping(input int tag);
    started = $time;
    got = tag;
    #DELAY;
    finished = $time;
    b.arrivals = b.arrivals + 1;
  endtask

  task b.Sum(input int tag, output int total, inout int count);
    #DELAY;
    total = tag + DELAY;
    count = count + 1;
  endtask
endmodule

module Top;
  Bus bus ();
  Provider #(.DELAY(3)) p1 (bus.provider);
  Provider #(.DELAY(7)) p2 (bus.provider);

  int resumed = -1;
  int total = 0;
  int count = 10;

  initial begin
    #1 bus.Ping(9);
    resumed = $time;
    bus.Sum(100, total, count);
  end

  final begin
    if (p1.started !== 1)
      $fatal(1, "p1.started was %0d, expected 1", p1.started);
    if (p2.started !== 1)
      $fatal(1, "p2.started was %0d, expected 1", p2.started);
    if (p1.finished !== 4)
      $fatal(1, "p1.finished was %0d, expected 4", p1.finished);
    if (p2.finished !== 8)
      $fatal(1, "p2.finished was %0d, expected 8", p2.finished);
    if (p1.got !== 9 || p2.got !== 9)
      $fatal(1, "the tags were %0d and %0d, expected 9", p1.got, p2.got);
    if (resumed !== 8) $fatal(1, "resumed was %0d, expected 8", resumed);
    if (bus.arrivals !== 2)
      $fatal(1, "bus.arrivals was %0d, expected 2", bus.arrivals);
    if (total !== 107) $fatal(1, "total was %0d, expected 107", total);
    if (count !== 11) $fatal(1, "count was %0d, expected 11", count);
    $display("All checks passed");
  end
endmodule
