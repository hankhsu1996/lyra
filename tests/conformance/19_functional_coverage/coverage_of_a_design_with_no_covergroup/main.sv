// A design that makes no covergroup instance has nothing to cover, so its
// overall coverage is 100 (LRM 19.11), and the coverage functions answer as
// they do in any other design (LRM 19.9).
module Top;
  real overall = -1.0;

  initial begin
    $set_coverage_db_name("never_written.xml");
    overall = $get_coverage();
  end

  final begin
    if (overall != 100.0) $fatal(1, "the overall coverage was %0f, expected 100", overall);
    $display("All checks passed");
  end
endmodule
