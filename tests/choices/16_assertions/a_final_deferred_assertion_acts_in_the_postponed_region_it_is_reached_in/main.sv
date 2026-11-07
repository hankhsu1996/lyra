// @reports: error line=12
//
// A final deferred assertion's report matures in the Postponed region, and its
// action is scheduled in the same Postponed region (LRM 16.4.1). Reached from a
// function a strobed write's argument calls, which Lyra evaluates in the
// Postponed region on no thread (LRM 21.2.2), the assertion is reached in that
// region itself, so its report matures and its action -- here the `$error` a
// failing assertion with no else clause calls -- runs there, in the time step
// that reached it.
module Top;
  function automatic int checks(int value);
    assert final (value == 0);
    return value;
  endfunction

  initial begin
    $display("Ready for the report");
    $strobe("%0d", checks(2));
    #1 $display("All checks passed");
  end
endmodule
