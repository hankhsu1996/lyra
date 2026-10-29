// process::self() returns a handle to the process making the call (LRM 9.7),
// and a static variable's declaration assignment is executed with no thread
// active (LRM 18.14.1), so no process makes a call written there. What self()
// answers then the standard does not say. Lyra answers null, the handle that
// names no object. This case holds Lyra to that.
module Top;
  bit initializer_ran;

  function automatic process self_from_an_initializer();
    initializer_ran = 1;
    return process::self();
  endfunction

  process from_initializer = self_from_an_initializer();

  final begin
    if (initializer_ran !== 1)
      $fatal(1, "the declaration assignment never ran");
    if (from_initializer != null)
      $fatal(1, "process::self() in a declaration assignment was not null");
    $display("All checks passed");
  end
endmodule
