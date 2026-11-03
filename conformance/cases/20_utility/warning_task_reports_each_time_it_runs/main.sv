// @reports: warning line=11 count=12
//
// A severity task generates its report each time it is called (LRM 20.10), so
// one call reached twelve times reports twelve times, each with the message
// its arguments format to. A warning leaves the run going, so the sentinel is
// printed after the last of them.
module Top;
  int ran = 0;
  initial begin
    for (int i = 0; i < 12; i++) begin
      $warning("pass %0d", i);
      ran++;
    end
    if (ran != 12) $fatal(1, "the loop ran %0d times, expected 12", ran);
    $display("All checks passed");
  end
endmodule
