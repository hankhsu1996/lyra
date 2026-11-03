// @reports: violation
//
// `$assertoff` is `$assertcontrol(4, 15, 7)`, whose types are the four
// assertion types and none of the violation reports (LRM 20.11, Table 20-6),
// so a `unique if` no condition of which holds still reports its violation
// (LRM 12.4.2).
module Top;
  int x = 3;
  int ran = 0;

  initial begin
    $assertoff;
    $display("Ready for the report");
    unique if (x == 1) ran = 1;
    else if (x == 2) ran = 2;
    #1;
    if (ran !== 0) $fatal(1, "an arm ran with no condition holding");
    $display("All checks passed");
  end
endmodule
