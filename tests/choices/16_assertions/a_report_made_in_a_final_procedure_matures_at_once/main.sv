// @reports: violation
// @reports: error line=19
//
// A violation report matures in the Observed region of its time step (LRM
// 12.4.2.1), and a final deferred assertion's report in the Postponed region,
// where its action then runs (LRM 16.4.1). A final procedure runs once the
// simulation is over (LRM 9.2.3), when no region runs at all, and the standard
// does not say what becomes of a report made there. Lyra matures each as it
// is made: nothing can flush it, since no region and no flush point follows,
// so it is the report its region would have issued.
module Top;
  int value = 2;

  final begin
    $display("Ready for the report");
    unique case (value)
      0: ;
    endcase
    assert final (value == 0);
    $display("All checks passed");
  end
endmodule
