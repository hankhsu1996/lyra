// @reports: error line=13 message="final saw 2"
//
// A final deferred assertion schedules its fail statement in the Postponed
// region (LRM 16.4), and the statement may be a system task. Its arguments are
// evaluated where the assertion is, so the report names the value the variable
// held then, not the one it holds when the time step ends.
module Top;
  int v;

  initial begin
    v = 2;
    $display("Ready for the report");
    assert final (v == 0) else $error("final saw %0d", v);
    v = 3;
    #1 $display("All checks passed");
  end
endmodule
