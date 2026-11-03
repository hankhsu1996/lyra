// @reports-none: violation
//
// `$assertcontrol` with no assertion_type selects every type, the violation
// reports of a `unique`, `unique0` or `priority` statement among them, and Off
// stops new ones being queued (LRM 20.11, Table 20-6). With reporting off, a
// `unique if` no condition of which holds issues no violation report (LRM
// 12.4.2).
module Top;
  int x = 3;
  int ran = 0;

  initial begin
    $assertcontrol(4);
    unique if (x == 1) ran = 1;
    else if (x == 2) ran = 2;
    #1;
    if (ran !== 0) $fatal(1, "an arm ran with no condition holding");
    $display("All checks passed");
  end
endmodule
