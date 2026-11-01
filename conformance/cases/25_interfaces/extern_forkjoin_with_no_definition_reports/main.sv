// @reports: error
//
// A task an interface declares `extern forkjoin` that no connected module
// defines is legal until it is called. A call then reports a run-time error and
// returns at once with no effect (LRM 25.7.4).
interface Bus;
  extern forkjoin task Ping();
endinterface

module Top;
  Bus bus ();

  initial begin
    #2;
    $display("Ready for the report");
    bus.Ping();
    if ($time !== 2) $fatal(1, "the call returned at %0t, expected 2", $time);
    $display("All checks passed");
  end
endmodule
