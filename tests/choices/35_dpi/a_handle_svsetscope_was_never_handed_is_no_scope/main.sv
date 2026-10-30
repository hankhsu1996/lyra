// @reports: error
//
// svSetScope makes the scope it is given current for what the foreign code
// does next (LRM 35.5.3, Annex H.9.3), and the standard says nothing of a
// handle that is no scope. The C layer gives out every scope handle foreign
// code can hold, so one it never gave out is no scope however it is passed on:
// installed by svSetScope it is what svGetScope then reports, and still
// refused by the utilities that take a scope, and an exported subroutine
// reached under it ends the run with an error rather than running in no scope
// at all.
module Top;
  import "DPI-C" context function int set_scope_never_handed_out();
  import "DPI-C" context function void call_under_a_scope_never_handed_out();
  export "DPI-C" function reached;

  int stays_no_scope;

  function void reached();
    $display("the export ran under a scope that is none");
  endfunction

  initial begin
    stays_no_scope = set_scope_never_handed_out();
    if (stays_no_scope !== 7)
      $fatal(1, "the installed handle's checks reported %0d, expected 7",
             stays_no_scope);
    $display("Ready for the report");
    call_under_a_scope_never_handed_out();
    $display("All checks passed");
  end
endmodule
