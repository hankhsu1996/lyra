// $stop suspends simulation (LRM 20.2), so in a non-interactive run the
// calling process does not continue past the call. Its optional argument (0,
// 1, or 2) selects which diagnostic message a tool prints and changes nothing
// else (LRM 20.2, Table 20-1). The subject here is that execution does not
// continue, so the case prints the sentinel and then places a $fatal past the
// call where nothing should reach: if execution wrongly continues, the
// non-zero exit fails the case before the sentinel is read.
module Top;
  initial begin
    $display("All checks passed");
    $stop(2);
    $fatal(1, "execution continued past $stop");
  end
endmodule
