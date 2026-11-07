// @reports: error line=13
//
// An observed deferred assertion's report matures in the Observed region of
// its time step and its action runs in the Reactive region (LRM 16.4.1). A
// strobed write's arguments are evaluated in the Postponed region, after both
// (LRM 21.2.2, 4.4.2.9), so an observed deferred assertion in a function one of
// them calls has no region left to mature and act in, and the standard gives
// it no meaning there. Lyra refuses it as the design's error, at the
// assertion, rather than run its action in a region whose rules the action was
// not written for.
module Top;
  function automatic int checks(int value);
    assert #0 (value == 0);
    return value;
  endfunction

  initial begin
    $display("Ready for the report");
    $strobe("%0d", checks(2));
    #1 $display("All checks passed");
  end
endmodule
