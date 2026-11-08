// @reports-none: error finish
//
// A variable's initializer runs before any procedure (LRM 10.5) and may reach
// an exported function that calls $finish (LRM 20.2) through an import. Where
// the simulator exits is the tool's; Lyra stops every execution at the task
// that ends the run, whether or not a process runs it, so nothing after the
// import call runs in the function the initializer called, and no procedure
// starts. The final procedures run (LRM 9.2.3).
module Top;
  import "DPI-C" context function void call_in();

  export "DPI-C" function ends_the_run;

  int after_the_call = 7;
  int a_procedure_ran = 7;

  function void ends_the_run();
    $finish(0);
  endfunction

  function int initialize();
    call_in();
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
