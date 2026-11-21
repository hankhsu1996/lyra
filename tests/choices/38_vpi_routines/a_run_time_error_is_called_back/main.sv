// @reports: error line=31
//
// LRM 38.36.3 calls an error callback when a run-time error occurs and lets it
// read the error through `vpi_chk_error`, leaving to the tool which errors
// those are. Lyra calls it for each run-time error it reports of the design --
// here a member of a class read through a handle naming no object (LRM 8.4) --
// at the level of an error and where the design wrote what failed, each
// callback reading it whatever routine one before it called. The run is
// already ending when the callbacks are called, as such an error ends it, so a
// time callback one asks for a later slot is refused. The final procedure
// checks what the callbacks read.
`timescale 1ns / 1ns

module Top;
  class Item;
    int n;
  endclass

  import "DPI-C" context function int place();
  import "DPI-C" context function int calls(int which);
  import "DPI-C" context function int level(int which);
  import "DPI-C" context function int line(int which);
  import "DPI-C" context function int later_slot_placed(int which);

  Item none;
  int read = 0;

  initial begin
    if (place() !== 0) $fatal(1, "placing the callbacks failed");
    $display("Ready for the report");
    read = none.n;
    $display("All checks passed");
  end

  // vpiError is 3 (LRM Annex K).
  final begin
    for (int which = 0; which < 2; ++which) begin
      if (calls(which) !== 1 || level(which) !== 3 || line(which) !== 31)
        $fatal(1, "error callback %0d: %0d calls, at level %0d, line %0d",
               which, calls(which), level(which), line(which));
      if (later_slot_placed(which) !== 0)
        $fatal(1, "error callback %0d placed a callback for a later slot",
               which);
    end
  end
endmodule
