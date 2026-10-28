// @reports: error line=13
//
// A deferred immediate assume carries the same simulation behavior as a
// deferred assert: its expression is evaluated where the statement is reached,
// and a false one with no fail statement reaches the tool's own report, held to
// a later region of the time step (LRM 16.4). The difference from assert is
// verification intent for a formal tool, not what simulation does, which is why
// the report names an assumption rather than an assertion. The sentinel a time
// step later says the run went on past the report.
module Top;
  initial begin
    $display("Ready for the report");
    assume #0 (0);
    #1 $display("All checks passed");
  end
endmodule
