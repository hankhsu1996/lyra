// Lyra has no `$reset` and no interactive mode, which LRM 38.36.3 counts among
// the features a VPI-compliant tool may lack. So `vpi_control` asked for
// `vpiReset` or `vpiSetInteractiveScope` answers 0 (38.4), and says so through
// `vpi_chk_error` as an error (LRM 38.2). A `vpi_control` asking for
// `$finish` from an import that is not `context`, called from SystemVerilog,
// has no call whose return it could be done upon (LRM 35.5.3), and answers 0
// the same way.
//
// `vpi_chk_error` reports the call before it and leaves the status as it found
// it, and the next routine clears it, so a successful call between two checks
// is what tells the second that nothing went wrong.
module Top;
  import "DPI-C" context function int reset_is_refused();
  import "DPI-C" context function int interactive_scope_is_refused();
  import "DPI-C" context function int a_check_reports_only_the_last_call();
  import "DPI-C" function int finish_outside_a_context_import_is_refused();

  int reset = -1;
  int interactive = -1;
  int status = -1;
  int plain = -1;

  initial begin
    reset = reset_is_refused();
    interactive = interactive_scope_is_refused();
    status = a_check_reports_only_the_last_call();
    plain = finish_outside_a_context_import_is_refused();
  end

  final begin
    if (reset !== 1) $fatal(1, "vpiReset was not refused as an error");
    if (interactive !== 1)
      $fatal(1, "vpiSetInteractiveScope was not refused as an error");
    if (status !== 1)
      $fatal(1, "vpi_chk_error did not report the last call alone");
    if (plain !== 1)
      $fatal(1, "a finish asked for outside a context import was not refused");
    $display("All checks passed");
  end
endmodule
