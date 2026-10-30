// @reports: error line=11
//
// A deferred assertion written outside procedural code is treated as if it
// were contained in an always_comb procedure (LRM 16.4.3), so it is evaluated
// again whenever what it reads changes. A value that is still false when the
// time step settles is reported, with no else clause by the default report of
// a failed assertion (LRM 16.4.1).
module Top;
  logic a;

  assert #0 (a !== 0);

  initial begin
    $display("Ready for the report");
    a = 1;
    #1 a = 0;
    #1 a = 1;
    $display("All checks passed");
  end
endmodule
