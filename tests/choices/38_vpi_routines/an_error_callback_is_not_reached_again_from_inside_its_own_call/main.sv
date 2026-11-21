// @reports: error
//
// LRM 38.36.3 calls a routine-error callback after a VPI routine fails and a
// run-time-error callback after a run-time error, and says nothing of one that
// fails from inside such a call. Lyra does not call a callback for an error
// from inside a call for that kind of error, however deep: here a
// routine-error callback calls an export, which no callback may (LRM Annex
// H.9), and the run-time error refusing it calls the run-time-error callback,
// whose own failing routine would otherwise call the routine-error callback
// again, and so on without end. Each is called once, and the run then ends as
// a run-time error ends it.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place_and_fail();
  import "DPI-C" context function int routine_errors();
  import "DPI-C" context function int run_time_errors();
  export "DPI-C" function poke;

  function int poke();
    return 1;
  endfunction

  initial begin
    $display("Ready for the report");
    if (place_and_fail() !== 0) $fatal(1, "placing the callbacks failed");
    $display("All checks passed");
  end

  final begin
    if (routine_errors() !== 1 || run_time_errors() !== 1)
      $fatal(1, "the error callbacks were called %0d and %0d times",
             routine_errors(), run_time_errors());
  end
endmodule
