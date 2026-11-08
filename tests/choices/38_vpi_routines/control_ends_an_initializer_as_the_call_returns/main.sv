// @reports-none: error finish
//
// `vpi_control(vpiFinish, n)` causes `$finish` to be executed upon return of
// the application routine (LRM 38.4), and a variable's initializer runs before
// any procedure (LRM 10.5) and may call a context import. Lyra executes the
// `$finish` as though it were written after the call, and stops every
// execution at a task that ends the run, whether or not a process runs it: so
// nothing after the call runs in the function the initializer called, and no
// procedure starts. The final procedures run (LRM 9.2.3).
module Top;
  import "DPI-C" context function void finish_from_foreign_code();

  int after_the_call = 7;
  int a_procedure_ran = 7;

  function int initialize();
    finish_from_foreign_code();
    after_the_call = 1;
    return 1;
  endfunction

  int initialized = initialize();

  initial a_procedure_ran = 1;

  final begin
    if (after_the_call !== 7)
      $fatal(1, "the statement after the import call ran");
    if (a_procedure_ran !== 7)
      $fatal(1, "a procedure started after the run ended");
    $display("All checks passed");
  end
endmodule
