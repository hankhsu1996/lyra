// @reports: error
//
// Once the Postponed region is reached, it is illegal to write a variable or
// to schedule an event in any earlier region of the time slot (LRM 4.4.2.9).
// A function a strobed write's argument calls runs there (LRM 21.2.2), and
// where its write wakes a process waiting on the variable, the wake would
// schedule into the Active region the slot has left. Lyra ends the run with the
// design's error once the region has run; the error names no line, since the
// write's own place is not known without a cost on every write.
module Top;
  int watched;

  function automatic int writes();
    watched = 5;
    return 3;
  endfunction

  initial begin
    $display("Ready for the report");
    $strobe("%0d", writes());
    #1 $display("All checks passed");
  end

  initial begin
    @(watched);
    $display("woke on %0d", watched);
  end
endmodule
