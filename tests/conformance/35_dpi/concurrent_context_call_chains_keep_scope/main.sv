// A call chain's context is created when SystemVerilog calls a context import
// and is equal to the instantiated scope of that import declaration
// (LRM 35.5.3). An imported task that calls an exported task which suspends
// leaves the chain part-way through while other threads run, so two instances
// of one module can have two chains alive at once (LRM 35.5.1.5). Each chain
// keeps its own context across the suspension: the scope a chain observes
// after resuming is its own instance's scope, named as such, and the export it
// reaches is still that instance's copy, not the copy belonging to whichever
// chain started later.
module Sub #(parameter int NAP = 10, parameter int TAG = 0,
             parameter string PATH = "");
  int tag;

  export "DPI-C" task nap_and_report;
  task nap_and_report(input int amount, output int reported);
    #amount;
    reported = tag;
  endtask

  // The instance's own hierarchical name travels with the call, so the foreign
  // side can hold the scope it observes against a named one rather than only
  // against the scope it observed a moment earlier.
  import "DPI-C" context task observe(
      input string path, input int amount, output int verdict);

  int verdict;
  int done_at;

  initial begin
    tag = TAG;
    verdict = -1;
    observe(PATH, NAP, verdict);
    done_at = $time;
  end
endmodule

module Top;
  // Both chains start at time zero and each suspends inside its own export,
  // so whichever ran first is still suspended when the other begins and the
  // naps differ in length, which makes them end at different times.
  Sub #(.NAP(10), .TAG(7), .PATH("Top.s_long")) s_long ();
  Sub #(.NAP(4), .TAG(3), .PATH("Top.s_short")) s_short ();

  final begin
    if (s_long.verdict !== 7)
      $fatal(1, "the long chain reported %0d, expected 7", s_long.verdict);
    if (s_short.verdict !== 3)
      $fatal(1, "the short chain reported %0d, expected 3", s_short.verdict);
    if (s_long.done_at !== 10)
      $fatal(1, "the long chain returned at time %0d, expected 10",
             s_long.done_at);
    if (s_short.done_at !== 4)
      $fatal(1, "the short chain returned at time %0d, expected 4",
             s_short.done_at);
    $display("All checks passed");
  end
endmodule
