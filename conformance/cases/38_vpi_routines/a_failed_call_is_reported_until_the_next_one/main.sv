// `vpi_chk_error` reports whether the VPI call before it failed (LRM 38.2):
// it answers the error's level, which is 0 where the call succeeded, and with
// a structure fills in the same level and a message. It leaves that status as
// it found it, so asking twice, or without a structure, answers the same, and
// the next routine called replaces it. Descriptor 1 cannot be closed (38.24),
// so `vpi_mcd_close(1)` is a call that fails, and answers the channel it left
// open.
module Top;
  import "DPI-C" context function int what_closing_one_answered();
  import "DPI-C" context function int the_failure_is_reported();
  import "DPI-C" context function int it_is_reported_again();
  import "DPI-C" context function int the_next_call_replaces_it();

  int left_open = -1;
  int reported = -1;
  int again = -1;
  int replaced = -1;

  initial begin
    left_open = what_closing_one_answered();
    reported = the_failure_is_reported();
    again = it_is_reported_again();
    replaced = the_next_call_replaces_it();
  end

  final begin
    if (left_open !== 1)
      $fatal(1, "vpi_mcd_close(1) answered %0d, expected 1", left_open);
    if (reported !== 1)
      $fatal(1, "vpi_chk_error did not report the failed call");
    if (again !== 1)
      $fatal(1, "a second vpi_chk_error did not report the same failure");
    if (replaced !== 1)
      $fatal(1, "the call after the failure did not clear the status");
    $display("All checks passed");
  end
endmodule
